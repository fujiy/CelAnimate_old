{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Reflex.Dom

import           Editor.Main

main :: IO ()
main = mainWidget editor
