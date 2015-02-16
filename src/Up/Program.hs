module Up.Program (
    main
) where

import Data.Char (toLower)
import Data.Function (on)
import Data.List (intercalate)
import Data.List.Split (splitOneOf)
import System.Directory (canonicalizePath)
import System.Environment (getArgs, getEnv, getProgName)
import System.FilePath (joinDrive, splitDrive, (</>), pathSeparator, dropExtension)
import Text.PrintOption
import Up.Options
import Up.ParseCmdLine
import qualified System.Exit


data ExitCode
    = Program_Success
    | Program_BadArgs
    | UpTo_BadDestination
    deriving (Enum, Eq)


exitWith :: ExitCode -> IO a
exitWith = System.Exit.exitWith . fromNumericCode . fromEnum
    where
        fromNumericCode 0 = System.Exit.ExitSuccess
        fromNumericCode n = System.Exit.ExitFailure n


main :: IO ()
main = do
    args <- getArgs
    case parseUpOptions args of
        Nothing -> runHelp >> exitWith Program_BadArgs
        Just res -> runOpt (separator res) (upOption res) >>= exitWith


-- NOTE: canonicalizePath expands symlinks
fixupPath :: FilePath -> IO FilePath
fixupPath = canonicalizePath


splitDirectories :: FilePath -> [String]
splitDirectories = splitOneOf "\\/"


useSeparator :: Separator -> FilePath -> FilePath
useSeparator sep = joiner . splitDirectories
    where
        joiner = intercalate $ case sep of
            ForwardSlash -> "/"
            BackSlash -> "\\"


runHelp :: IO ExitCode
runHelp = do
    let maxWidth = 80
    let line = putStrLn $ replicate maxWidth '-'
    let settings = defaultPrintSettings { maxColumn = maxWidth }
    let p = printOptionWith settings
    line
    progName <- fmap dropExtension getProgName
    putStrLn $ "Usage: " ++ progName ++ " [OPTIONS]"
    putStrLn $ "Emits a path to go up a certain amount of directories based on OPTIONS."
    line
    putStrLn $ "Options:"
    p ' ' "help" "Display this help and exit."
    p 'r' "relative" "Emit the path as a relative path."
    p 'a' "absolute" "Emit the path as an absolute path."
    p 's' "separator (forward|back)" "The type of file separator to emit as. If this option is not supplied, the emitted separator is implementation defined but will be compliant with the program's operating system."
    p 'i' "ignore-case" "Ignore case when matching strings."
    p 'm' "from-by DIRECTORY AMOUNT" "Goes up DIRECTORY by AMOUNT. Emits as an absolute path by default."
    p 'n' "by AMOUNT" "Goes up the current directory by AMOUNT. Emits as a relative path by default."
    p 'f' "from-to DIRECTORY PATHPART" "Goes up DIRECTORY until you hit PATHPART. Emits as an absolute path by default."
    p 't' "to PATHPART" "Goes up the current directory until you hit PATHPART. Emits as a relative path by default."
    line
    putStrLn $ fit maxWidth $ "In the above options, AMOUNT can be of two forms. (1) a non-negative integer. (2) a series of consecutive '.' (dot) characters. Case (2) is the same as (1) if AMOUNT were the number of dots minus one."
    line
    putStrLn $ fit maxWidth $ "The following flags do not need to be explicitly written and can be implied: --by, --to. If AMOUNT is supplied, --by is chosen. In all other cases, --to is chosen."
    line
    putStrLn $ fit maxWidth $ "If multiple options are supplied and any of them conflict, the rightmost option takes precedence unless any of the conflicting options is defined by an implicit flag. In that case, it is a usage error."
    return Program_Success


getWorkingDir :: IO String
getWorkingDir = getEnv "PWD" -- not using getCurrentDirectory because symlinks would get expanded


runUpBy :: Separator -> Maybe FilePath -> PathType -> Int -> IO ExitCode
runUpBy sep mDir pt n = do
    dir <- case mDir of
        Just dir -> return dir
        Nothing -> fixupPath =<< getWorkingDir
    let relPath = upBy n
    putStr . useSeparator sep =<< case pt of
        RelativePath -> return relPath
        AbsolutePath -> fixupPath $ dir </> relPath
    return Program_Success


runUpTo :: Separator -> Maybe FilePath -> PathType -> Bool -> String -> IO ExitCode
runUpTo sep mDir pt ic part = do
    dir <- case mDir of
        Just dir -> return dir
        Nothing -> fixupPath =<< getWorkingDir
    case upTo ic dir part of
        Nothing ->
            return UpTo_BadDestination
        Just relPath -> do
            putStr . useSeparator sep =<< case pt of
                RelativePath -> return relPath
                AbsolutePath -> fixupPath $ dir </> relPath
            return Program_Success


runOpt :: Maybe Separator -> UpOption -> IO ExitCode
runOpt mSep opt = case opt of
    Help -> runHelp
    UpBy mDir pt n -> runUpBy sep mDir pt n
    UpTo mDir pt ic str -> runUpTo sep mDir pt ic str
    where
        sep = maybe defaultSep id mSep
        defaultSep = case pathSeparator of
            '\\' -> BackSlash
            '/' -> ForwardSlash
            _ -> ForwardSlash


mkPath :: String -> [String] -> FilePath
mkPath defaultPath parts = case intercalate "/" parts of
    "" -> defaultPath
    path -> path


upBy :: Int -> FilePath
upBy = mkPath "." . flip replicate ".."


upTo :: Bool -> FilePath -> String -> Maybe FilePath
upTo ic path = upTo' ic $ splitDirectories path


upTo' :: Bool -> [String] -> String -> Maybe FilePath
upTo' ic targets target = case targets' of
    [] -> Nothing
    _ -> Just $ upBy $ length targets - length targets'
    where
        targets' = reverse . dropWhile pred . reverse $ targets
        pred = if ic
            then on (/=) (map toLower) target
            else (/= target)











