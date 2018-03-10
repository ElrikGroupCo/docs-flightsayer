{-# LANGUAGE OverloadedStrings #-}
import Turtle
import Turtle.Pattern

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as LText
import Filesystem
import Control.Monad

data Token = TypeSignature Text
           | TypeSignatureMeta Text Text deriving Show

main = do
  rawLines <- readTextFile . (</> "status.md") =<< pwd
  rs <- mapM (\line -> liftM (match typeSignature) (pure line)) (Text.lines rawLines)
  mapM print (concatMap id rs)
  where
    typeSignature = do
      char '#'
      char '#'
      space
      r <- liftM (TypeSignature .  Text.pack) (many alphaNum)
      space
      char '('
      meta <- many (noneOf ")")
      char ')'
      return r
  
