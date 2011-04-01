{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Arrow ((>>>))
import Control.Monad (forM_)

import Hakyll

main :: IO ()
main = hakyll $ do
    -- Read templates
    compile "templates/*" templateCompiler
    
    -- Compress CSS
    route   "css/*" idRoute
    compile "css/*" compressCssCompiler
    
    -- Archive downloads
    route   "downloads/*" idRoute
    compile "downloads/*" copyFileCompiler
    
    -- Documentation pages
    forM_ [ "index.md"
          , "license.md"
          ] $ \page -> do
        route   page $ setExtension ".html"
        compile page $ pageCompiler
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler
