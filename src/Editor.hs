{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Editor where

import qualified Data.ByteString as BS
import qualified Data.Text       as T
import qualified Data.Text.IO    as T
import           JSDOM.Types
import           Reflex.Dom




type Editor = JSM

class Editable a where
    type Viewer a :: * -> *

    open   :: FilePath -> Editor (Maybe a)
    create :: Editor a
    save   :: FilePath -> a -> Editor ()
    match  :: FilePath -> a -> Bool

    viewer :: a -> Viewer a ()

    match path a = True



class EditableBinary a where
    reader :: BS.ByteString -> Maybe a
    writer :: a -> BS.ByteString

    bempty :: a

newtype BinaryFile a = BinaryFile { unBinaryFile :: a }

class EditableText a where
    parser  :: T.Text -> Maybe a
    printer :: a -> T.Text

    tempty :: a

newtype TextFile a = TextFile { unTextFile :: a }

-- instance EditableBinary a => Editable (BinaryFile a) where
--     open       = fmap (fmap BinaryFile . reader) . BS.readFile
--     create path = do
--         let init = BinaryFile bempty
--         save path init
--         return $ Just init
--     save path = BS.writeFile path . writer . unBinaryFile



-- instance EditableText a => Editable (TextFile a) where
--     open       = fmap (fmap TextFile . parser) . T.readFile
--     create path = do
--         let init = TextFile tempty
--         save path init
--         return $ Just init
--     save path = T.writeFile path . printer . unTextFile

