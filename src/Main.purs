module Main where

import Prelude

import Control.Monad.Aff (launchAff, Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array (filterM)
import Data.Maybe (maybe, Maybe(..))
import Data.Either (Either(..))
import Data.String (charAt, singleton, lastIndexOf)
import Node.FS (FS)
import Node.FS.Aff (stat, readdir)
import Node.FS.Stats (isDirectory)
import Node.Yargs.Applicative (flag, yarg, runY)
import Node.Yargs.Setup (example, usage)
import Data.Array (uncons)

app :: forall eff. Array String -> Eff ( err     :: EXCEPTION
                                                  , fs      :: FS
                                                  , console :: CONSOLE | eff) Unit
app []     = pure unit
app directories =
  case uncons directories of
    Just { head: x, tail: xs } ->
      void $ launchAff do
        files <- readdir x
        files' <- filterM keepVisibleDirs files
        liftEff $ log $ show files'
    Nothing ->
      pure unit

main :: forall eff. Eff ( err     :: EXCEPTION
                        , fs      :: FS
                        , console :: CONSOLE
                        | eff
                        ) Unit
main = do
  let setup = usage "$0 -d path-to-directory-to-scan"

  runY setup $ app <$> yarg "d" ["directory"] (Just "Path to the directory to scan for markdown files") (Right "At least one word is required") false
                  --  <*> flag "r" [] (Just "Reverse the words")

keepVisibleDirs :: forall eff. String -> Aff ( fs :: FS
                                             | eff
                                             )
                                             Boolean
keepVisibleDirs file = do
  stat <- stat file
  pure $ not (isDirectory stat)
    && (maybe false (singleton >>> (_ /= ".")) $ charAt 0 file)
    && (maybe false (\_ -> true) (lastIndexOf ".md" file))
