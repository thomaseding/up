module Up.Options (
      UpOption(..)
    , PathType(..)
    , Separator(..)
    ) where


data Separator = ForwardSlash | BackSlash
    deriving (Show, Eq)


data PathType
    = AbsolutePath
    | RelativePath
    deriving (Show, Eq)


data UpOption
    = Help
    | UpBy {
          basePath :: Maybe FilePath
        , pathType :: PathType
        , amount :: Int
        }
    | UpTo {
          basePath :: Maybe FilePath
        , pathType :: PathType
        , ignoreCase :: Bool
        , target :: String
        }
    deriving (Show, Eq)









