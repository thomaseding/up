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
import qualified System.Exit


data ExitCode
    = Program_Success
    | Program_BadArgs
    | Program_UpTo_BadDestination
    | Program_UnknownFailure
    deriving (Enum, Eq)


exitWith :: ExitCode -> IO a
exitWith = System.Exit.exitWith . fromNumericCode . fromEnum
    where
        fromNumericCode 0 = System.Exit.ExitSuccess
        fromNumericCode n = System.Exit.ExitFailure n


main :: IO ()
main = do
    args <- getArgs
    case parseOptions defaultSep args of
        Left msg -> do
            putStrLn msg
            exitWith Program_BadArgs
        Right options -> do
            exitWith =<< runUp options


runUp :: UpOptions -> IO ExitCode
runUp options = case optionHelp options of
    Just msg -> do
        putStrLn msg
        return Program_Success
    Nothing -> do
        let sep = optionSeparator options
            mDir = optionBasePath options
            ic = optionIgnoreCase options
            fixPT pt = case optionPathType options of
                Nothing -> pt
                Just pt' -> pt'
        case optionUpBy options of
            Just (pt, n) -> runUpBy sep mDir (fixPT pt) n
            Nothing -> case optionUpTo options of
                Just (pt, str) -> runUpTo sep mDir (fixPT pt) ic str
                Nothing -> do
                    putStrLn "*** Internal logic error! ***"
                    return Program_UnknownFailure


defaultSep :: Separator
defaultSep = case pathSeparator of
    '\\' -> BackSlash
    '/' -> ForwardSlash
    _ -> ForwardSlash


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


getWorkingDir :: IO String
getWorkingDir = getEnv "PWD" -- not using getCurrentDirectory because symlinks would get expanded


runUpBy :: Separator -> BasePath -> PathType -> Int -> IO ExitCode
runUpBy sep mDir pt n = do
    dir <- case mDir of
        GivenPath dir -> return dir
        CurrPath -> fixupPath =<< getWorkingDir
    let relPath = upBy n
    putStr . useSeparator sep =<< case pt of
        RelativePath -> return relPath
        AbsolutePath -> fixupPath $ dir </> relPath
    return Program_Success


runUpTo :: Separator -> BasePath -> PathType -> Bool -> String -> IO ExitCode
runUpTo sep mDir pt ic part = do
    dir <- case mDir of
        GivenPath dir -> return dir
        CurrPath -> fixupPath =<< getWorkingDir
    case upTo ic dir part of
        Nothing ->
            return Program_UpTo_BadDestination
        Just relPath -> do
            putStr . useSeparator sep =<< case pt of
                RelativePath -> return relPath
                AbsolutePath -> fixupPath $ dir </> relPath
            return Program_Success


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











