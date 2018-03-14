{-# LANGUAGE OverloadedStrings #-}
import Turtle
import Turtle.Pattern

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as LText
import Filesystem
import Control.Monad

data Token = TypeSignature Text
           | TypeDescription Text
           | TypeAttribute Text
           | TypeRequest Text
           | TypeResponse Text
           | TypeSignatureMeta Text Text deriving Show

-- zipWith (a -> b -> c) [a] [b] [c]

main = do
  rawLines <- readTextFile . (</> "status.md") =<< pwd
  rs <- mapM (return . zipWith ($) programs . repeat)
             (Text.lines rawLines)
  mapM print (concatMap id rs)
  where
    programs = fmap match [typeSignature
                          ,typeDescription
                          ,typeRequest]
    typeSignature,typeDescription,typeRequest :: Pattern Token
    typeSignature = do
      char '#'
      char '#'
      space
      r <- liftM Text.pack (many alphaNum)
      space
      char '('
      meta <- liftM Text.pack (many (noneOf ")"))
      char ')'
      return (case Text.null meta of
        True  -> TypeSignature r
        False -> TypeSignatureMeta r meta)

    typeRequest  = do
      char '+'
      space
      r <- char 'R'
      e <- char 'e'
      q <- char 'q'
      uestBody <- many alphaNum
      return $ TypeRequest (Text.pack $ [r,e,q]++uestBody)

    typeDescription = do
     description <- many (noneOf "#")
     (case null description of
       True -> mzero
       False -> liftM TypeDescription (pure (Text.pack description)))
