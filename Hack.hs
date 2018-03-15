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
           | TypeRequestHeader
           | TypeResponseHeader (Either Int Int)
           | TypeParamHeader
           | TypeBodyHeader
           | TypeParamSymbol Text Text Bool
           | TypeParamSymbolII Text Text Bool
           | TypeSignatureMeta Text Text deriving Show

-- zipWith (a -> b -> c) [a] [b] [c]

main = do
  rawLines <- readTextFile . (</> "status.md") =<< pwd
  rs <- mapM (return . zipWith ($) programs . repeat)
             (Text.lines rawLines)
  mapM print (concatMap (Data.List.foldl' (\b a -> case length a of
                                                   0 -> b
                                                   1 -> b ++ a
                                                   _ -> b ++ tail a ) []) rs)
  where
    programs = fmap match [typeParamSymbolII
                          ,typeParamSymbol
                          ,typeParamHeader
                          ,typeBodyHeader
                          ,typeRequestHeader
                          ,typeResponseHeader
                          ,typeSignature
                          ,typeDescription]

    typeSignature :: Pattern Token
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

    typeRequestHeader  = plusPrefix >> text (Text.pack "Request")    >> pure TypeRequestHeader
    typeResponseHeader = char '+'  >> space >> text (Text.pack "Response") >> space >> responseParser

    responseParser =   (do
                         char '4' >>  (pure $ TypeResponseHeader (Left 400)) <* many anyChar)
                      <|>
                       (do
                         char '2' >>  (pure (TypeResponseHeader (Right 200)) <* many anyChar))
    typeParamHeader    = plusPrefix >> text (Text.pack "Parameters") >> pure TypeParamHeader
    typeBodyHeader     = plusPrefix >> text (Text.pack "Body")       >> pure TypeBodyHeader
    plusPrefix         = optional (skip (many space)) >> char '+' >> space

    typeParamSymbol = do
                        (skip spaces) >> char '+' >> space
                        sseq <- many (noneOf ":")
                        char ':'
                        sdesc <- many (noneOf "-")
                        liftM3 TypeParamSymbol (pure (Text.pack sseq))
                                               (pure (Text.pack sdesc))
                                               (pure ("required" `Data.List.isInfixOf` sseq))
    typeParamSymbolII = do
                        (skip spaces) >> char '+' >> space
                        sseq <- many (noneOf "-")
                        char '-'
                        liftM3 TypeParamSymbolII (pure (Text.pack sseq))
                                                 (liftM Text.pack  (many anyChar))
                                                 (pure ("required" `Data.List.isInfixOf` sseq))
    typeDescription = do
     description <- many (noneOf "#")
     (case null description of
       True -> mzero
       False -> liftM TypeDescription (pure (Text.pack description)))
