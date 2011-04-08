{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Arrow ((>>>))
import Control.Monad (forM_)

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
    
    -- Documentation pages
    forM_ [ "index.md"
          , "license.md"
          ] $ \page -> do
        match page $ do
            route   $ setExtension ".html"
            compile $ pageCompiler
                >>> applyTemplateCompiler "templates/default.html"
                >>> relativizeUrlsCompiler
