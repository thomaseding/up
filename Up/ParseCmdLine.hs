{-# LANGUAGE FlexibleContexts #-}

module Up.ParseCmdLine (
      parseUpOptions
    , ParseResult(upOption, separator)
    ) where


import Up.Options
import Control.Monad
import Control.Monad.State
import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.String


data ParseResult = ParseResult {
      upOption :: UpOption
    , separator :: Maybe Separator
    }


updatePathType :: PathType -> UpOption -> UpOption
updatePathType pathType opt = case opt of
    Help -> Help
    upBy @ UpBy{} -> upBy { pathType = pathType }
    upTo @ UpTo{} -> upTo { pathType = pathType }


data ParseState = ParseState {
      upOptions :: [UpOption]
    , mPathType :: Maybe PathType
    , mSeparator :: Maybe Separator
    , stateIgnoreCase :: Bool
    , hasImplicitOption :: Bool
    }
    deriving (Show)


tryRead :: (Read a) => String -> Maybe a
tryRead str = case reads str of
    [(val, "")] -> Just val
    _ -> Nothing


liftRunParser :: (Monad m, Stream s Identity t) => Parsec s u a -> u -> s -> m a
liftRunParser state parser stream = case runParser state parser "" stream of
    Left err -> fail $ show err
    Right res -> return res


liftParse :: (Monad m, Stream s Identity tok) => Parsec s () a -> s -> m a
liftParse parser = liftRunParser parser ()


genSatisfy :: (Show tok, Stream s m tok) => (tok -> Bool) -> ParsecT s st m tok
genSatisfy pred = tokenPrim show updatePos $ \t -> if pred t then Just t else Nothing
    where
        updatePos pos _ _ = incSourceColumn pos 1


consume :: (Eq tok, Show tok, Stream s m tok) => tok -> ParsecT s st m tok
consume x = genSatisfy (== x)


consumeAny :: (Eq tok, Show tok, Stream s m tok) => [tok] -> ParsecT s st m tok
consumeAny xs = genSatisfy (`elem` xs)


parseNonNegInt :: GenParser Char st Int
parseNonNegInt = do
    ds <- many1 digit
    return $ read ds


parseDots :: GenParser Char st Int
parseDots = fmap (\dots -> length dots - 1) $ many1 $ char '.'


parseAmount :: GenParser Char st Int
parseAmount = parseNonNegInt <|> parseDots


parseHelp :: GenParser String st UpOption
parseHelp = consume "--help" >> return Help


parseRelative :: GenParser String st PathType
parseRelative = consumeAny ["--relative", "-r"] >> return RelativePath


parseAbsolute :: GenParser String st PathType
parseAbsolute = consumeAny ["--absolute", "-a"] >> return AbsolutePath


parsePathType :: GenParser String st PathType
parsePathType = parseRelative <|> parseAbsolute


parseFromBy :: GenParser String st UpOption
parseFromBy = do
    consumeAny ["--from-by", "-m"]
    dir <- anyToken
    n <- anyToken >>= liftParse parseAmount
    return $ UpBy (Just dir) AbsolutePath n


parseBy :: Bool -> GenParser String ParseState UpOption
parseBy implicit = do
    unless implicit $ consumeAny ["--by", "-n"] >> return ()
    n <- anyToken >>= liftParse parseAmount
    when implicit $ modifyState $ \st -> st { hasImplicitOption = True }
    return $ UpBy Nothing RelativePath n


parseFromTo :: GenParser String st UpOption
parseFromTo = do
    consumeAny ["--from-to", "-f"]
    dir <- anyToken
    dest <- anyToken
    return $ UpTo (Just dir) AbsolutePath False dest


parseTo :: Bool -> GenParser String ParseState UpOption
parseTo implicit = do
    unless implicit $ consumeAny ["--to", "-t"] >> return ()
    dest <- anyToken
    when implicit $ modifyState $ \st -> st { hasImplicitOption = True }
    return $ UpTo Nothing RelativePath False dest


parseUpOption :: GenParser String ParseState UpOption
parseUpOption = parseHelp
    <|> parseFromBy
    <|> parseFromTo
    <|> parseBy False
    <|> parseTo False
    <|> try (parseBy True)
    <|> parseTo True


parseSpecificSeparator :: String -> Separator -> GenParser String st Separator
parseSpecificSeparator name sep = do
    consumeAny ["--separator", "-s"]
    consume name
    return sep


parseSeparator :: GenParser String st Separator
parseSeparator = do
    consumeAny ["--separator", "-s"]
    name <- consumeAny ["forward", "back"]
    case name of
        "forward" -> return ForwardSlash
        "back" -> return BackSlash
        _ -> fail "Internal error"


parseIgnoreCase :: GenParser String st ()
parseIgnoreCase = consumeAny ["--ignore-case", "-i"] >> return ()


parseOption :: GenParser String ParseState ()
parseOption = do
    res <- option False $ do
        pt <- parsePathType
        modifyState $ \st -> st { mPathType = Just pt }
        return True
    res <- if res
        then return True
        else option False $ do 
            sep <- parseSeparator
            modifyState $ \st -> st { mSeparator = Just sep }
            return True
    res <- if res
        then return True
        else option False $ do
            parseIgnoreCase
            modifyState $ \st -> st { stateIgnoreCase = True }
            return True
    unless res $ do
        upOpt <- parseUpOption
        modifyState $ \st -> st { upOptions = upOpt : upOptions st }


parseOptions :: GenParser String ParseState ParseState
parseOptions = many1 parseOption >> eof >> getState


parseUpOptions :: [String] -> Maybe ParseResult
parseUpOptions args = case runParser parseOptions initState "" $ filter (not . null) args of
    Left err -> Nothing
    Right st -> constructOpt st
    where
        initState = ParseState {
              upOptions = []
            , mPathType = Nothing
            , mSeparator = Nothing
            , stateIgnoreCase = False
            , hasImplicitOption = False
            }
        constructOpt st = case pickBest (hasImplicitOption st) $ upOptions st of
            Nothing -> Nothing
            Just Help -> Just ParseResult { upOption = Help, separator = Nothing }
            Just upOpt -> Just $ case mPathType st of
                Nothing -> ParseResult {
                      upOption = updateIgnoreCase upOpt
                    , separator = mSeparator st
                    }
                Just pathType -> ParseResult {
                      upOption = updateIgnoreCase $ updatePathType pathType upOpt
                    , separator = mSeparator st
                    }
            where
                updateIgnoreCase opt = if stateIgnoreCase st 
                    then case opt of
                        Help -> Help
                        upBy @ UpBy{} -> upBy
                        UpTo mDir pt _ part -> UpTo mDir pt True part
                    else opt


pickBest :: Bool -> [UpOption] -> Maybe UpOption
pickBest hasImplicitOpt opts = if Help `elem` opts
    then Just Help
    else if hasImplicitOpt
        then case opts of
            [opt] -> Just opt
            _ -> Nothing
        else case opts of
            [] -> Nothing
            opt : _ -> Just opt
















