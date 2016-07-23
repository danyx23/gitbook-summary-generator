module Main where

import Prelude

import Control.Monad.Aff (launchAff, Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array (filterM)
import Data.Maybe (maybe)
import Data.String (charAt, singleton, lastIndexOf)
import Node.FS (FS)
import Node.FS.Aff (stat, readdir)
import Node.FS.Stats (isDirectory)

main :: forall eff. Eff ( err     :: EXCEPTION
                        , fs      :: FS
                        , console :: CONSOLE
                        | eff
                        ) Unit
main = void $ launchAff do
  files <- readdir "."
  files' <- filterM keepVisibleDirs files
  liftEff $ log $ show files'

keepVisibleDirs :: forall eff. String -> Aff ( fs :: FS
                                             | eff
                                             )
                                             Boolean
keepVisibleDirs file = do
  stat <- stat file
  pure $ not (isDirectory stat)
    && (maybe false (singleton >>> (_ /= ".")) $ charAt 0 file)
    && (maybe false (\_ -> true) (lastIndexOf ".md" file))
