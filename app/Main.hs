{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{-|
Module      : Main
Description : This module implements the main loop with the GUI

Compile with: stack ghc -- Main.hs

Load packages with: stack ghci --package <package_name>
              Example of four first moves:
              play (b 4 4) [] >>= play (w 16 16) >>= play (b 3 16) >>= play (w 16 4)
-}

module Main where

import GUI

main :: IO ()
main = mainGoGUI