module Main where

import Grammar ( getFirstTable, emptyReg, firstState, getGotoTable )
import Parser ( parseGrammar )
import Control.Monad.State (runState, evalState)

main :: IO ()
main = do
  res <- readFile "ata.bnk"
  case parseGrammar res of
    Left err -> putStrLn err
    Right grammar -> do
      let fst' = getFirstTable grammar
      let (n, res') = runState (getGotoTable grammar) (emptyReg fst')
      print fst'
      putStr "\n\n"
      print res'