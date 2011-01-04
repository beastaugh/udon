module Main (main) where

import Text.Hakyll (hakyll)
import Text.Hakyll.File (directory)
import Text.Hakyll.Render (renderChain, static)
import Text.Hakyll.CreateContext (createPage)

main = hakyll "http://extralogical.net/projects/udon" $ do
  static "udon.css"
  directory static "downloads"
  
  mapM_ (renderChain [ "templates/default.html" ] . createPage)
          [ "index.md"
          , "license.md"
          ]
