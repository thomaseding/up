{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}

module Up.Options (
    UpOptions(..),
    PathType(..),
    Separator(..),
    BasePath(..),
    parseOptions,
) where


import Control.Monad.State.Strict
import Data.List
import Data.Typeable
import Text.LambdaOptions


--------------------------------------------------------------------------------


data Separator = ForwardSlash | BackSlash
    deriving (Typeable)


data PathType
    = AbsolutePath
    | RelativePath


data BasePath
    = CurrPath
    | GivenPath FilePath


data UpOptions = UpOptions {
    optionHelp :: Maybe String,
    optionBasePath :: BasePath,
    optionUpBy :: Maybe (PathType, Int),
    optionUpTo :: Maybe (PathType, FilePath),
    optionSeparator :: Separator,
    optionPathType :: Maybe PathType,
    optionIgnoreCase :: Bool,
    optionActionCount :: Int
} deriving ()


--------------------------------------------------------------------------------


newtype DotInt = DotInt { unDotInt :: Int }
    deriving (Typeable)


instance Parseable DotInt where
    parse args = case args of
        [] -> (Nothing, 0)
        arg : _ -> case span (== '.') arg of
            (dots@('.' : _), []) -> (Just $ DotInt $ length dots - 1, 1)
            _ -> case parse args of
                (Just num, n) -> (Just $ DotInt num, n)
                _ -> (Nothing, 0)


instance Parseable Separator where
    parse = \case
        [] -> (Nothing, 0)
        arg : _ -> case arg of
            "forward" -> (Just ForwardSlash, 1)
            "backward" -> (Just BackSlash, 1)
            _ -> (Nothing, 0)


--------------------------------------------------------------------------------


incActionCount :: State UpOptions ()
incActionCount = modify $ \st -> st { optionActionCount = optionActionCount st + 1 }


options :: Options (State UpOptions ())
options = do
    let done = return () :: State UpOptions ()
    addOption (kw ["--help", "-h"] `text` "Display this help message.") $ do
        modify $ \st -> st { optionHelp = Just usage }
        done
    addOption (kw ["--relative", "-r"] `text` "Emit the path as a relative path.") $ do
        modify $ \st -> st { optionPathType = Just RelativePath }
        done
    addOption (kw ["--absolute", "-a"] `text` "Emit the path as an absolute path.") $ do
        modify $ \st -> st { optionPathType = Just AbsolutePath }
        done
    addOption (kw ["--separator", "-s"] `argText` "(forward|back)" `text` "The type of file separator to emit. If this option is not supplied, the emitted separator will be compliant with the program's operating system.") $ \sep -> do
        modify $ \st -> st { optionSeparator = sep }
        done
    addOption (kw ["--ignore-case", "-i"] `text` "Ignore case when matching strings.") $ do
        modify $ \st -> st { optionIgnoreCase = True }
        done
    addOption (kw ["--from-by", "-m"] `argText` "DIR AMOUNT" `text` "Goes up DIR by AMOUNT. Emits an absolute path by default.") $ \dir n -> do
        modify $ \st -> st { optionBasePath = GivenPath dir, optionUpBy = Just (AbsolutePath, unDotInt n) }
        incActionCount
        done
    addOption (kw ["--by", "-n"] `argText` "AMOUNT" `text` "Goes up the current directory by AMOUNT. Emits a relative path by default.") $ \n -> do
        modify $ \st -> st { optionBasePath = CurrPath, optionUpBy = Just (RelativePath, unDotInt n) }
        incActionCount
        done
    addOption (kw ["--from-to", "-f"] `argText` "DIR PATHPART" `text` "Goes up DIR until PATHPART is hit. Emits an absolute path by default.") $ \dir part -> do
        modify $ \st -> st { optionBasePath = GivenPath dir, optionUpTo = Just (AbsolutePath, part) }
        incActionCount
        done
    addOption (kw ["--to", "-t"] `argText` "PATHPART" `text` "Goes up the current directory until PATHPART is hit. Emits a relative path by default.") $ \dir part -> do
        modify $ \st -> st { optionBasePath = GivenPath dir, optionUpTo = Just (RelativePath, part) }
        incActionCount
        done


helpDesc :: String
helpDesc = getHelpDescription options


usage :: String
usage = intercalate "\n" [
    fit "Usage: up [OPTION]",
    fit "Emits a path to go up a certain amount of directories based on [OPTION].",
    fit line,
    helpDesc,
    fit line,
    fit "In the above options, AMOUNT can be of two forms. (1) a non-negative integer. (2) a series of consecutive '.' (dot) characters. Case (2) is the same as (1) if AMOUNT were the number of dots minus one.",
    fit line,
    fit "The following flags do not need to be explicitly written and can be implied: --by, --to. If AMOUNT is supplied, --by is chosen. In all other cases, --to is chosen." ]
    where
        line = replicate 80 '-'
        fit = format defaultFormatConfig


--------------------------------------------------------------------------------


initialState :: Separator -> UpOptions
initialState sep = UpOptions {
    optionHelp = Nothing,
    optionBasePath = CurrPath,
    optionUpBy = Nothing,
    optionUpTo = Nothing,
    optionSeparator = sep,
    optionPathType = Nothing,
    optionIgnoreCase = False,
    optionActionCount = 0 }


parseOptions :: Separator -> [String] -> Either String UpOptions
parseOptions sep args = case parseOptions' sep args of
    Left msg -> Left msg
    Right [opts] -> Right opts
    Right _ -> Left usage


parseOptions' :: Separator -> [String] -> Either String [UpOptions]
parseOptions' sep args = case runOptions options args of
    Left err -> Left $ (prettyOptionsError err) ++ "\n" ++ usage
    Right actions -> mapM runAction actions
    where
        runAction action = case execState action $ initialState sep of
            st -> case optionHelp st of
                Just _ -> Right st
                Nothing -> case optionActionCount st of
                    1 -> Right st
                    0 -> Left usage
                    _ -> Left $ "Please only supply one mode at a time.\n" ++ usage


