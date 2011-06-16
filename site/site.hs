{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main where

import Control.Arrow ((>>>))

import Hakyll

main :: IO ()
main = hakyll $ do
    -- Read templates
    match "templates/*" $ compile templateCompiler
    
    -- Compress CSS
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler
    
    -- Archive downloads
    match "downloads/*" $ do
        route   idRoute
        compile copyFileCompiler
    
    -- Documentation
    match "index.md" $ do
        route   $ setExtension ".html"
        compile $ pageCompiler
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler
