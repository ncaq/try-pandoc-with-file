{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Home ( getHomeR, postHomeR ) where

import           Data.FileEmbed
import qualified Data.Text             as T
import           Import
import           System.FilePath
import           Text.Pandoc
import           Text.Pandoc.PDF
import           Text.Pandoc.UTF8      as U
import           Yesod.Form.Bootstrap3

getHomeR :: Handler Html
getHomeR = do
    (form, enctype) <- generateFormPost pandocForm
    defaultLayout $ do
        setTitle "try pandoc with file"
        $(widgetFile "homepage")

postHomeR :: Handler TypedContent
postHomeR = do
    ((result, _), _) <- runFormPost pandocForm
    case result of
        FormMissing -> invalidArgs ["FormMissing"]
        FormFailure msg -> invalidArgs ("FormFailure" : msg)
        FormSuccess PandocForm{ .. } -> do
            fileBytes <- fileSourceBytes file
            let ePandoc = case reader of
                    MarkdownStrict -> readMarkdownStrict (U.toString fileBytes)
            case ePandoc of
                Left m -> invalidArgs ("ePandoc" : lines (tshow m))
                Right pandoc -> case writer of
                    Pdf -> do
                        epdf <- liftIO $ makePDF "/usr/bin/lualatex" writeLaTeX
                            laTeXWriterOptions
                            pandoc
                        case epdf of
                            Left m -> invalidArgs ("epdf" : lines (toStrict (decodeUtf8 m)))
                            Right pdf -> do
                                addHeaderContentDisposition
                                    (T.pack (dropExtension (T.unpack (fileName file))) <> ".pdf")
                                sendResponse ("application/pdf" :: ContentType, toContent pdf)
                    LaTeX -> do
                        addHeaderContentDisposition
                                (T.pack (dropExtension (T.unpack (fileName file))) <> ".tex")
                        sendResponse ( "application/x-tex" :: ContentType
                                     , toContent $ writeLaTeX laTeXWriterOptions pandoc
                                     )

data PandocForm = PandocForm
    { file   :: FileInfo
    , reader :: PandocReader
    , writer :: PandocWriter
    }

data PandocReader = MarkdownStrict
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

data PandocWriter = Pdf | LaTeX
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

pandocForm :: Form PandocForm
pandocForm = renderBootstrap3 BootstrapBasicForm $ PandocForm <$>
    areq fileField "file" Nothing <*>
    areq (selectField optionsEnum) "reader" Nothing <*>
    areq (selectField optionsEnum) "writer" Nothing <*
    bootstrapSubmit ("convert" :: BootstrapSubmit Text)

readMarkdownStrict :: String -> Either PandocError Pandoc
readMarkdownStrict = readMarkdown def
    { readerExtensions = strictExtensions
    , readerStandalone = True
    }

laTeXWriterOptions :: WriterOptions
laTeXWriterOptions = def
    { writerTemplate = Just $(embedStringFile "templates/pdf.sty")
    , writerListings = True
    }

-- | Bytestring 'fileSource'
fileSourceBytes :: MonadResource m => FileInfo -> m ByteString
fileSourceBytes fileInfo = fileSource fileInfo $$ foldC

-- | set filename
addHeaderContentDisposition :: MonadHandler m => Text -> m ()
addHeaderContentDisposition fileName = addHeader "Content-Disposition"
    ("attachment; filename*=UTF-8''" <> decodeUtf8 (urlEncode False (encodeUtf8 fileName)))
