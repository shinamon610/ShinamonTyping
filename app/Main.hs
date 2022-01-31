{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Main where

import qualified MyLib (someFunc)
import System.Random
import Control.Monad (replicateM)
import System.IO
import System.Process
import System.Info
import NoBufferingWorkaround

main :: IO ()
main = do
  putStrLn "Welcome to ShinamonTyping"
  modeloop

modeloop = do
  putStrLn "* 1: Japanese"
  putStrLn "* 2: English"
  putStrLn "* 3: Symbols"
  putStrLn "* 4: quit"
  initGetCharNoBuffering 
  s <- getCharNoBuffering
  let num = 100
  case s of
    '1' -> do
      gen <- getStdGen
      let xs = ['あ', 'い', 'う', 'え' ,'お', 'か', 'き', 'く', 'け', 'こ', 'さ', 'し', 'す', 'せ', 'そ', 'た', 'ち', 'つ', 'て', 'と', 'な', 'に', 'ぬ', 'ね', 'の', 'は', 'ひ', 'ふ', 'へ', 'ほ', 'ま', 'み', 'む', 'め', 'も', 'や', 'ゆ', 'よ', 'わ', 'を', 'ん', 'ぁ', 'ぃ', 'ぅ', 'ぇ', 'ぉ', 'ゃ', 'ゅ', 'ょ', 'ー', 'が', 'ぎ', 'ぐ', 'げ', 'ご', 'ざ', 'じ', 'ず', 'ぜ', 'ぞ', 'だ', 'ぢ', 'づ', 'で', 'ど', 'ば', 'び', 'ぶ', 'べ', 'ぼ', 'ぱ', 'ぴ', 'ぷ', 'ぺ', 'ぽ']
      let s = take num $ map (xs !!) $ randomRs (0, length xs - 1) gen
      inputloop s
      modeloop
    '2' -> do
      s <- replicateM num (randomRIO ('a', 'z'))
      inputloop s
      modeloop
    '3'-> do
      gen <- getStdGen
      let xs = ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '!', '@', '#', '$', '%', '^', '&', '*', '_', '+', ':', '|', '\"', '(', ')', '-', '=', ';', '/', '\'', '<', '>', '`', '?', '{', '}', '~', '¥', '[', ']', '\\']
      let s = take num $ map (xs !!) $ randomRs (0, length xs - 1) gen
      inputloop s
      modeloop
    '4' -> return()
    _ -> modeloop

inputloop::String -> IO()
inputloop w = do
  clear
  case w of
    [] -> do
      putStrLn "end"
      return ()
    ww -> do
      putStrLn ww
      s <- getCharNoBuffering
      if s == head ww then inputloop (tail ww) else inputloop ww

clear = do
  case os of
    "darwin" -> system "clear"
    "mingw32"->system "cls"