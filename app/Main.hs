{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Lib

import           Reflex.Dom

main :: IO ()
main = mainWidget $ el "div" $ text "Welcome to Reflex"
