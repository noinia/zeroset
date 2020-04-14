module Main where

import ZeroSet.Main(mainWith, options)
import Options.Applicative


main :: IO ()
main = execParser options >>= mainWith
