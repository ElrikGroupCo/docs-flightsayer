{-# LANGUAGE OverloadedStrings #-}
import Turtle
import Turtle.Pattern
import Pipes
import qualified Pipes.Prelude as Pipes

-- notes
-- https://hackage.haskell.org/package/pipes-4.3.9/docs/Pipes-Prelude.html
-- http://www.scs.stanford.edu/11au-cs240h/notes/performance.html#(32)
-- https://wiki.haskell.org/ListT_done_right
-- https://github.com/ElrikGroupCo/docs-flightsayer/blob/hack/Output.txt

-- concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- concat :: Foldable t => t [a] -> [a]

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.List

import Filesystem
import Control.Monad

data Token = TypeSignature Text
           | TypeDescription Text
           | TypeAttributeHeader
           | TypeAttributeNameTypeRequired Text Text Bool
           | TypeRequestHeader
           | TypeResponseHeader (Either Text Text)
           | TypeParamHeader
           | TypeBodyHeader
           | TypeParamSymbol Text Text Bool
           | TypeParamSymbolII Text Text Bool
           | TypeSignatureMeta Text Text deriving (Eq,Show)

newtype Body = Body Text deriving (Show,Eq)
data SwaggerSpec = NoSpec
                 | SwaggerRequestHeaderHead Token SwaggerSpec
                 | SwaggerResponseHeaderHead Token SwaggerSpec
                 | SwaggerRequest  Token [Token] SwaggerSpec
                 | SwaggerResponse Token [Token] SwaggerSpec
                 | SwaggerRequestBodyEmpty Token [Token] SwaggerSpec
                 | SwaggerRequestBody Token [Token] Body SwaggerSpec
                 | SwaggerResponseBodyEmpty Token [Token] SwaggerSpec
                 | SwaggerResponseBody Token [Token] Body SwaggerSpec
                 deriving (Eq,Show)

main = do
  rawLines <- readTextFile . (</> "status.md") =<< pwd
  rs <- (concatMap concat)
        <$>
        mapM (return . zipWith ($) programs . repeat) (Text.lines rawLines)

  swaggerSpec <- Pipes.foldM (\x a -> return $ toSpec x a)
                             (pure NoSpec)
                             return
                             (each rs)

  print swaggerSpec

  where
    toSpec l                                  r@TypeRequestHeader                    = SwaggerRequestHeaderHead  r l
    toSpec l                                  r@(TypeResponseHeader _)               = SwaggerResponseHeaderHead r l

    toSpec l@(SwaggerRequestHeaderHead h tl)  TypeParamHeader                        = l
    toSpec l@(SwaggerRequestHeaderHead h tl) r@(TypeParamSymbolII _ _ _)             = SwaggerRequest h [r] tl
    toSpec l@(SwaggerRequestHeaderHead h tl) r@(TypeParamSymbol   _ _ _)             = SwaggerRequest h [r] tl
    toSpec l@(SwaggerRequestHeaderHead h tl) r@(TypeAttributeNameTypeRequired _ _ _) = SwaggerRequest h [r] tl


    toSpec l@(SwaggerRequest h rs tl)        r@(TypeParamSymbolII _ _ _)             = SwaggerRequest h (r:rs) tl
    toSpec l@(SwaggerRequest h rs tl)        r@(TypeParamSymbol   _ _ _)             = SwaggerRequest h (r:rs) tl
    toSpec l@(SwaggerRequest h rs tl)           TypeBodyHeader                       = SwaggerRequestBodyEmpty h rs tl
    toSpec l@(SwaggerRequest h rs tl)        r@(TypeAttributeNameTypeRequired _ _ _) = SwaggerRequest h (r:rs) tl
    toSpec l@(SwaggerRequestBodyEmpty h rs tl) (TypeDescription t)                   = SwaggerRequestBody h rs (Body t) tl
    toSpec l@(SwaggerRequestBody h rs (Body ta) tl)   (TypeDescription tb)           = SwaggerRequestBody h rs (Body (Text.append ta tb)) tl

    toSpec l@(SwaggerResponseHeaderHead h tl) TypeParamHeader = l
    toSpec l@(SwaggerResponseHeaderHead h tl) r@(TypeParamSymbolII _ _ _)            = SwaggerResponse h [r] tl
    toSpec l@(SwaggerResponseHeaderHead h tl) r@(TypeParamSymbol   _ _ _)            = SwaggerResponse h [r] tl
    toSpec l@(SwaggerResponseHeaderHead h tl) r@(TypeAttributeNameTypeRequired _ _ _)= SwaggerResponse h [r] tl
    toSpec l@(SwaggerResponse h rs tl)        r@(TypeParamSymbolII _ _ _)            = SwaggerResponse h (r:rs) tl
    toSpec l@(SwaggerResponse h rs tl)        r@(TypeParamSymbol   _ _ _)            = SwaggerResponse h (r:rs) tl
    toSpec l@(SwaggerResponse h rs tl)        r@(TypeAttributeNameTypeRequired _ _ _)= SwaggerResponse h (r:rs) tl
    toSpec l@(SwaggerResponse h rs tl)        TypeBodyHeader                         = SwaggerResponseBodyEmpty h rs tl
    toSpec l@(SwaggerResponseBodyEmpty h rs tl) (TypeDescription t)                  = SwaggerResponseBody h rs (Body t) tl
    toSpec l@(SwaggerResponseBody h rs (Body ta) tl)   (TypeDescription tb)          = SwaggerResponseBody h rs (Body (Text.append ta tb)) tl



    toSpec b      _                                                     = b

    programs = fmap match [typeAttributeNameTypeRequired
                          ,typeParamSymbolII
                          ,typeParamSymbol
                          ,typeParamHeader
                          ,typeBodyHeader
                          ,typeAttributeHeader
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

    typeAttributeHeader= plusPrefix >> text ("Attributes") >> (pure TypeAttributeHeader) <* many  anyChar

    typeAttributeNameTypeRequired= do
                           (skip spaces) >> char '+' >> space
                           attrName <- many (noneOf "(")
                           char '('
                           attrType <- many (noneOf ",")
                           char ','
                           attrReq <- (text (" required") *> pure True)
                                     <|>
                                      (text (" optional") *> pure False)
                                     <|>
                                      (pure False)
                           char ')'
                           liftM3 TypeAttributeNameTypeRequired
                                  (liftM Text.pack (pure attrName))
                                  (liftM Text.pack (pure attrType))
                                  (pure attrReq)

    typeRequestHeader  = plusPrefix >> text ("Request")    >> pure TypeRequestHeader
    typeResponseHeader = char '+'  >> space >> text ("Response") >> space >> responseParser <* space <* many anyChar

    responseParser =   (do
                         char '4'
                         liftM (TypeResponseHeader . Left . Text.pack . ('4':)) (many digit))
                      <|>
                       (do
                         char '2'
                         liftM (TypeResponseHeader . Right . Text.pack . ('2':)) (many digit))

    typeParamHeader    = plusPrefix >> text ("Parameters") >> pure TypeParamHeader
    typeBodyHeader     = plusPrefix >> text ("Body")       >> pure TypeBodyHeader
    plusPrefix         = skip (many space) >> char '+' >> space

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
                                                 (liftM Text.pack (many anyChar))
                                                 (pure ("required" `Data.List.isInfixOf` sseq))
    typeDescription = do
     description <- many (noneOf "#+")
     (case null description of
       True -> mzero
       False -> liftM (TypeDescription . Text.pack) (pure  description))
