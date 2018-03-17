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
-- search = Cmd + G

-- concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- concat :: Foldable t => t [a] -> [a]

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.List

import Filesystem
import Control.Monad

data Token = TypeSignature      Text
           | TypeDescription    Text
           | TypeAttributeHeader
           | TypeRequestHeader
           | TypeResponseHeader (Either Text Text)
           | TypeParamHeader
           | TypeBodyHeader
           | TypeFormatHeader
           | TypeDataStructureHeader
           | TypeMemberHeader
           | TypeDataStructure  Text Text
           | TypeParamSymbol    Text Text Bool
           | TypeParamSymbolII  Text Text Bool
           | TypeParamSymbolIII Text Text Bool
           | TypeSignatureMeta  Text Text
           | TypeRoute          Text
           deriving (Eq,Show)

newtype Body = Body Text deriving (Eq,Show)
newtype Type = Type Text deriving (Eq,Show)

data SwaggerSpec = NoSpec
                 | SwaggerAttributeHeader SwaggerSpec
                 | SwaggerRequestHeaderHead SwaggerSpec
                 | SwaggerResponseHeaderHead SwaggerSpec
                 | SwaggerRequest  [Token] SwaggerSpec
                 | SwaggerResponse [Token] SwaggerSpec
                 | SwaggerRequestBodyEmpty  [Token] SwaggerSpec
                 | SwaggerRequestBody [Token] Body SwaggerSpec
                 | SwaggerResponseBodyEmpty [Token] SwaggerSpec
                 | SwaggerResponseBody [Token] Body SwaggerSpec
                 | SwaggerRoute Text SwaggerSpec
                 | SwaggerFormat SwaggerSpec
                 | SwaggerDataStructureHeader SwaggerSpec
                 | SwaggerDataStructure Text Type SwaggerSpec
                 | SwaggerEntity Text SwaggerSpec
                 | SwaggerCollectionEntity Text
                 | SwaggerMemberCollection [SwaggerSpec] SwaggerSpec
                 deriving (Eq,Show)

main = do
  rawLines <- liftM2 (<>)
                     (readTextFile . (</> "status.md") =<< pwd)
                     (readTextFile . (</> "flight_subscriptions.md") =<< pwd)
  swaggerSpec <-
              (Data.List.foldl' toSpec NoSpec)
              <$>
              ((concatMap concat)
               <$>
               mapM (return . zipWith ($) programs . repeat)
                    (Text.lines rawLines))

  print swaggerSpec

  where
    toSpec l                                  TypeDataStructureHeader                = SwaggerDataStructureHeader l
    toSpec l                                  TypeFormatHeader                       = SwaggerFormat              l
    toSpec l                                  (TypeRoute t)                          = SwaggerRoute             t l
    toSpec l                                  TypeRequestHeader                      = SwaggerRequestHeaderHead   l
    toSpec l                                  (TypeResponseHeader _)                 = SwaggerResponseHeaderHead  l
    toSpec l                                  TypeAttributeHeader                    = SwaggerAttributeHeader     l
    toSpec l                                  TypeMemberHeader                       = SwaggerMemberCollection [] l


    toSpec (SwaggerDataStructureHeader l) (TypeParamSymbolII  name typ _)           = SwaggerDataStructureHeader (SwaggerDataStructure name (Type typ) l)
    toSpec (SwaggerDataStructureHeader l) (TypeParamSymbol    name typ _)           = SwaggerDataStructureHeader (SwaggerDataStructure name (Type typ) l)
    toSpec (SwaggerDataStructureHeader l) (TypeParamSymbolIII name typ _)           = SwaggerDataStructureHeader (SwaggerDataStructure name (Type typ) l)

    toSpec l@(SwaggerRequestHeaderHead tl) r@(TypeParamSymbolII _ _ _)             = SwaggerRequest [r] tl
    toSpec l@(SwaggerRequestHeaderHead tl) r@(TypeParamSymbol   _ _ _)             = SwaggerRequest [r] tl
    toSpec l@(SwaggerRequestHeaderHead tl) r@(TypeParamSymbolIII _ _ _)            = SwaggerRequest [r] tl

    toSpec l@(SwaggerRequestBodyEmpty rs tl)        (TypeDescription t)            = SwaggerRequestBody rs (Body t) tl
    toSpec l@(SwaggerRequestBody rs (Body ta) tl)   (TypeDescription tb)           = SwaggerRequestBody rs (Body (ta <> tb)) tl

    toSpec l@(SwaggerRequest rs tl)        r@(TypeParamSymbolII _ _ _)             = SwaggerRequest (r:rs) tl
    toSpec l@(SwaggerRequest rs tl)        r@(TypeParamSymbol   _ _ _)             = SwaggerRequest (r:rs) tl
    toSpec l@(SwaggerRequest rs tl)        r@(TypeParamSymbolIII _ _ _)            = SwaggerRequest (r:rs) tl
    toSpec l@(SwaggerRequest rs tl)           TypeBodyHeader                       = SwaggerRequestBodyEmpty rs tl


    toSpec l@(SwaggerResponseHeaderHead tl) r@(TypeParamSymbolII _ _ _)            = SwaggerResponse [r] tl
    toSpec l@(SwaggerResponseHeaderHead tl) r@(TypeParamSymbol   _ _ _)            = SwaggerResponse [r] tl
    toSpec l@(SwaggerResponseHeaderHead tl) r@(TypeParamSymbolIII _ _ _)           = SwaggerResponse [r] tl

    toSpec l@(SwaggerResponseBodyEmpty rs tl)        (TypeDescription t)           = SwaggerResponseBody rs (Body t) tl
    toSpec l@(SwaggerResponseBody rs (Body ta) tl)   (TypeDescription tb)          = SwaggerResponseBody rs (Body (ta <> tb)) tl

    toSpec l@(SwaggerResponse rs tl)        r@(TypeParamSymbolII _ _ _)            = SwaggerResponse (r:rs) tl
    toSpec l@(SwaggerResponse rs tl)        r@(TypeParamSymbol   _ _ _)            = SwaggerResponse (r:rs) tl
    toSpec l@(SwaggerResponse rs tl)        r@(TypeParamSymbolIII _ _ _)           = SwaggerResponse (r:rs) tl
    toSpec l@(SwaggerResponse rs tl)        TypeBodyHeader                         = SwaggerResponseBodyEmpty rs tl

    toSpec (SwaggerMemberCollection ms tl) r@(TypeParamSymbolII  t _ _)              =
           SwaggerMemberCollection ((SwaggerCollectionEntity t):ms) tl
    toSpec (SwaggerMemberCollection ms tl) r@(TypeParamSymbol    t _ _)              =
           SwaggerMemberCollection ((SwaggerCollectionEntity  t):ms) tl
    toSpec (SwaggerMemberCollection ms tl) r@(TypeParamSymbolIII t _ _)              =
           SwaggerMemberCollection ((SwaggerCollectionEntity  t):ms) tl

    toSpec l                                  r@(TypeParamSymbolII  t _ _)           = SwaggerEntity t l
    toSpec l                                  r@(TypeParamSymbol    t _ _)           = SwaggerEntity t l
    toSpec l                                  r@(TypeParamSymbolIII t _ _)           = SwaggerEntity t l
    toSpec l                                  r@(TypeSignature t)                    = SwaggerEntity t l
    toSpec l                                  r@(TypeSignatureMeta ta tm)            = SwaggerEntity (ta <> " " <> tm) l
    toSpec b      _                                                                  = b

    programs = fmap match [typeRoute
                          ,typeParamSymbolIII
                          ,typeParamSymbolII
                          ,typeParamSymbol
                          ,typeParamHeader
                          ,typeBodyHeader
                          ,typeAttributeHeader
                          ,typeRequestHeader
                          ,typeResponseHeader
                          ,typeSignature
                          ,typeDescription
                          ,typeFormatHeader
                          ,typeDataStructureHeader
                          ,typeMemberHeader]

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

    typeAttributeHeader= plusPrefix    *> text ("Attributes") *> pure TypeAttributeHeader
    typeMemberHeader   = text "### Members"                   *> pure TypeMemberHeader
    typeFormatHeader   = text "FORMAT" *> char ':'            *> pure TypeFormatHeader
    typeRequestHeader  = plusPrefix *> text ("Request")       *> pure TypeRequestHeader
    typeResponseHeader = char '+' *> space >> text "Response" *> space *> responseParser

    responseParser = (<|>)
                     (char '4' *> liftM (TypeResponseHeader . Left . Text.pack . ('4':)) (many digit))
                     (char '2' *> liftM (TypeResponseHeader . Right . Text.pack . ('2':)) (many digit))

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
    typeParamSymbolIII= do
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
                           liftM3 TypeParamSymbolIII
                                  (liftM Text.pack (pure attrName))
                                  (liftM Text.pack (pure attrType))
                                  (pure attrReq)
    typeDataStructureHeader = text "# Data Structures" *> pure TypeDataStructureHeader
    typeDescription = do
     description <- many (noneOf "#+")
     (case null description of
       True -> mzero
       False -> liftM (TypeDescription . Text.pack) (pure  description))

    typeRoute = do
      skip spaces
      char '*'
      skip (many space)
      liftM (TypeRoute . Text.pack) (many anyChar)