-- Copyright 2009 Corey O'Connor
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.Vty.LLInput ( Key(..)
                            , Modifier(..)
                            , Button(..)
                            , Event(..)
                            , initTermInput 
                            ) 
    where

import Data.Char
import Data.Maybe ( mapMaybe 
                  )
import Data.List( inits )
import Data.Word
import qualified Data.Map as M( fromList, lookup )
import qualified Data.Set as S( fromList, member )

import Codec.Binary.UTF8.Generic (decode)

import Control.Monad (when)
import Control.Concurrent
import Control.Exception

import System.Console.Terminfo

import System.Posix.Signals.Exts
import System.Posix.Signals
import System.Posix.Terminal
import System.Posix.IO ( stdInput
                        ,fdRead
                        ,setFdOption
                        ,FdOption(..)
                       )

-- |Representations of non-modifier keys.
data Key = KEsc | KFun Int | KPrtScr | KPause | KASCII Char | KBS | KIns
         | KHome | KPageUp | KDel | KEnd | KPageDown | KNP5 | KUp | KMenu
         | KLeft | KDown | KRight | KEnter 
    deriving (Eq,Show,Ord)

-- |Modifier keys.  Key codes are interpreted such that users are more likely to
-- have Meta than Alt; for instance on the PC Linux console, 'MMeta' will
-- generally correspond to the physical Alt key.
data Modifier = MShift | MCtrl | MMeta | MAlt 
    deriving (Eq,Show,Ord)

-- |Mouse buttons.  Not yet used.
data Button = BLeft | BMiddle | BRight 
    deriving (Eq,Show,Ord)

-- |Generic events.
data Event = EvKey Key [Modifier] | EvMouse Int Int Button [Modifier]
           | EvResize Int Int 
    deriving (Eq,Show,Ord)

data KClass = Valid Key [Modifier] | Invalid | Prefix | MisPfx Key [Modifier] [Char]
    deriving(Show)

-- | Set up the terminal for input.  Returns a function which reads key
-- events, and a function for shutting down the terminal access.
initTermInput :: Int -> Terminal -> IO (IO Event, IO ())
initTermInput escDelay terminal = do
  eventChannel <- newChan
  inputChannel <- newChan
  hadInput <- newEmptyMVar
  oattr <- getTerminalAttributes stdInput
  let nattr = foldl withoutMode oattr [StartStopOutput, KeyboardInterrupts,
                                       EnableEcho, ProcessInput]
  setTerminalAttributes stdInput nattr Immediately
  set_term_timing
  let inputToEventThread :: IO ()
      inputToEventThread = loop [] 
          where loop kb = case (classify kb) of
                              Prefix       -> do c <- readChan inputChannel
                                                 loop (kb ++ [c])
                              Invalid      -> loop ""
                              MisPfx k m s -> writeChan eventChannel (EvKey k m) >> loop s
                              Valid k m    -> writeChan eventChannel (EvKey k m) >> loop ""

      finishAtomicInput = writeChan inputChannel '\xFFFE'

      inputThread :: IO ()
      inputThread = loop
          where 
              loop = do
                  setFdOption stdInput NonBlockingRead False
                  threadWaitRead stdInput
                  setFdOption stdInput NonBlockingRead True
                  try readAll 
                  when (escDelay == 0) finishAtomicInput
                  loop
              readAll = do
                  (bytes, bytes_read) <- fdRead stdInput 1
                  when (bytes_read > 0) $ do
                      tryPutMVar hadInput () -- signal input
                      writeChan inputChannel (head bytes)
                  readAll
      -- | If there is no input for some time, this thread puts '\xFFFE' in the
      -- inputChannel. 
      noInputThread :: IO ()
      noInputThread = when (escDelay > 0) loop
            where loop = do
                    takeMVar hadInput -- wait for some input
                    threadDelay escDelay -- microseconds
                    hadNoInput <- isEmptyMVar hadInput -- no input yet?
                    when hadNoInput $ do
                        finishAtomicInput
                    loop
      

      compile :: [[([Char],(Key,[Modifier]))]] -> [Char] -> KClass
      compile lst = cl' where
          lst' = concat lst
          pfx = S.fromList $ concatMap (init . inits . fst) $ lst'
          mlst = M.fromList lst'
          cl' str = case S.member str pfx of
                      True -> Prefix
                      False -> case M.lookup str mlst of
                                 Just (k,m) -> Valid k m
                                 Nothing -> case head $ mapMaybe (\s -> (,) s `fmap` M.lookup s mlst) $ init $ inits str of
                                              (s,(k,m)) -> MisPfx k m (drop (length s) str)
      
      -- ANSI specific bits


      classify, classifyTab :: [Char] -> KClass
      
      -- As soon as
      classify "\xFFFE" = Invalid
      classify s@(c:_) | ord c >= 0xC2 = 
          if utf8Length (ord c) > length s then Prefix else classifyUtf8 s -- beginning of an utf8 sequence
      classify other = classifyTab other

      classifyUtf8 s = case decode ((map (fromIntegral . ord) s) :: [Word8]) of
          Just (unicodeChar, _) -> Valid (KASCII unicodeChar) []
          _ -> Invalid -- something bad happened; just ignore and continue.
         
      classifyTab = compile (caps_classify_table : ansi_classify_table)
   
      caps_tabls = [("khome", (KHome, [])), 
                    ("kend",  (KEnd,  [])),

                    ("kcud1",  (KDown,  [])),
                    ("kcuu1",  (KUp,  [])),
                    ("kcuf1",  (KRight,  [])),
                    ("kcub1",  (KLeft,  []))
                   ]
   
      caps_classify_table = [(x,y) | (Just x,y) <- map (first (getCapability terminal . tiGetStr)) $ caps_tabls]
      
      ansi_classify_table :: [[([Char], (Key, [Modifier]))]]
      ansi_classify_table =
         [ let k c s = ("\ESC["++c,(s,[])) in
           [ k "G" KNP5, k "P" KPause ],
           let k n s = ("\ESC["++show n++"~",(s,[])) in zipWith k [2::Int,3,5,6] [KIns,KDel,KPageUp,KPageDown],
           [ (x:[],(KASCII x,[])) | x <- map toEnum [0..255] ],
           [ ("\ESC[["++[toEnum(64+i)],(KFun i,[])) | i <- [1..5] ],
           let f ff nrs m = [ ("\ESC["++show n++"~",(KFun (n-(nrs!!0)+ff), m)) | n <- nrs ] in
           concat [ f 6 [17..21] [], f 11 [23,24] [], f 1 [25,26] [MShift], f 3 [28,29] [MShift], f 5 [31..34] [MShift] ],
           [ ('\ESC':[x],(KASCII x,[MMeta])) | x <- '\ESC':'\t':[' ' .. '\DEL'] ],
           [ ([toEnum x],(KASCII y,[MCtrl])) 
              | (x,y) <- zip ([0..31]) ('@':['a'..'z']++['['..'_']),
                y /= 'i' -- Resolve issue #3 where CTRL-i hides TAB.
           ],
           [ ('\ESC':[toEnum x],(KASCII y,[MMeta,MCtrl])) | (x,y) <- zip [0..31] ('@':['a'..'z']++['['..'_']) ],
           [ ("\ESC",(KEsc,[])) , ("\ESC\ESC",(KEsc,[MMeta])) , ("\DEL",(KBS,[])), ("\ESC\DEL",(KBS,[MMeta])),
             ("\ESC\^J",(KEnter,[MMeta])), ("\^J",(KEnter,[])) ] ]
   
  eventThreadId <- forkIO $ inputToEventThread
  inputThreadId <- forkIO $ inputThread
  noInputThreadId <- forkIO $ noInputThread
  let pokeIO = (Catch $ do let e = error "(getsize in input layer)"
                           setTerminalAttributes stdInput nattr Immediately
                           writeChan eventChannel (EvResize e e))
  installHandler windowChange pokeIO Nothing
  installHandler continueProcess pokeIO Nothing
  let uninit = do killThread eventThreadId
                  killThread inputThreadId
                  killThread noInputThreadId
                  installHandler windowChange Ignore Nothing
                  installHandler continueProcess Ignore Nothing
                  setTerminalAttributes stdInput oattr Immediately
  return (readChan eventChannel, uninit)

first f (x,y) = (f x, y)


utf8Length c 
    | c < 0x80 = 1
    | c < 0xE0 = 2
    | c < 0xF0 = 3
    | otherwise = 4

foreign import ccall "set_term_timing" set_term_timing :: IO ()

