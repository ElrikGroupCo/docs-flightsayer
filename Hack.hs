{-# LANGUAGE OverloadedStrings #-}
import Turtle
import Turtle.Pattern

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as LText
import qualified Data.List
import Filesystem
import Control.Monad

data Token = TypeSignature Text
           | TypeDescription Text
           | TypeAttribute Text
           | TypeRequest Text
           | TypeResponse Text
           | TypeParamHeader
           | TypeParamSymbol Text Text Bool
           | TypeSignatureMeta Text Text deriving Show

-- zipWith (a -> b -> c) [a] [b] [c]

main = do
  rawLines <- readTextFile . (</> "status.md") =<< pwd
  rs <- mapM (return . zipWith ($) programs . repeat)
             (Text.lines rawLines)
  mapM print (filter (not . null) $ (concatMap id rs))
  where
    programs = fmap match [typeParamSymbol
                          ,typeParamHeader
                          ,typeResponse
                          ,typeRequest
                          ,typeSignature
                          ,typeDescription]

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
      uestBody <- many anyChar
      liftM TypeRequest (pure $ Text.pack $ [r,e,q]  ++ uestBody)

    typeResponse = do
      skip spaces
      char '+'
      space
      r <- char 'R'
      e <- char 'e'
      s <- char 's'
      ponseBody <- many anyChar
      liftM TypeResponse (pure $ Text.pack $ [r,e,s]  ++ ponseBody)

    typeParamHeader = skip spaces >> char '+' >> space >> text (Text.pack "Parameters") >> pure TypeParamHeader
    typeParamSymbol = do
                        skip spaces
                        char '+'
                        space
                        sseq <- many (noneOf ":")
                        char ':'
                        sdesc <- many (noneOf "-")
                        liftM3 TypeParamSymbol (pure (Text.pack sseq))
                                               (pure (Text.pack sdesc))
                                               (pure ("required" `Data.List.isInfixOf` sseq))
    typeDescription = do
     description <- many (noneOf "#")
     (case null description of
       True -> mzero
       False -> liftM TypeDescription (pure (Text.pack description)))
