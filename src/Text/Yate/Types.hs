{-# LANGUAGE CPP #-}

module Text.Yate.Types
  ( Path(..)
  , Template(..)
  , YateValue(..)
  , ToYate(..)
  ) where

#ifndef NoAeson
import           Data.Scientific     (toRealFloat)
import qualified Data.Aeson          as A
#endif
import qualified Data.HashMap.Strict as M
import qualified Data.Text           as T
import qualified Data.Text.Lazy      as TL
import qualified Data.Vector         as V

-- | Path to traverse in a 'YateValue' (e.g. @author.address.city@)
data Path
  = AbsolutePath [T.Text] -- ^ e.g. @thing.stuff@
  | RelativePath [T.Text] -- ^ e.g. @.stuff@ (see @in@ and @forall@ statements)
  deriving (Eq)

-- For error messages
instance Show Path where
  show path = case path of
    AbsolutePath names ->       T.unpack (T.intercalate "." names)
    RelativePath names -> '.' : T.unpack (T.intercalate "." names)

-- | Template which needs a given type of input data
data Template a
  = Content TL.Text                   -- ^ Raw content, nothing special
  | Variable Path                     -- ^ @=@ statement
  | If Path (Template a) (Template a) -- ^ @if@/@else@ statement
  | For T.Text Path (Template a)      -- ^ @for x in xs@ statement
  | In Path (Template a)              -- ^ @in@ statement
  | Parts [Template a]                -- ^ Template parts following each other
  deriving (Show, Eq)

-- | Data to feed to a template
data YateValue
  = String T.Text
  | Number Double
  | Object (M.HashMap T.Text YateValue)
  | List   (V.Vector YateValue)
  | Bool   Bool
  | Null
  deriving (Show, Eq)

class ToYate a where
  toYate :: a -> YateValue

instance ToYate YateValue where
  toYate = id

instance ToYate () where
  toYate () = Object $ M.empty

instance ToYate a => ToYate (V.Vector a) where
  toYate = List . fmap toYate

instance ToYate a => ToYate [a] where
  toYate = toYate . V.fromList

instance ToYate T.Text where
  toYate = String

instance ToYate Double where
  toYate = Number

instance ToYate Bool where
  toYate = Bool

instance ToYate a => ToYate (Maybe a) where
  toYate = maybe Null toYate

#ifndef NoAeson
instance ToYate A.Value where
  toYate x = case x of
    A.Object obj -> Object $ fmap toYate obj
    A.Array  arr -> List   $ fmap toYate arr
    A.String str -> String str
    A.Number num -> Number $ toRealFloat num
    A.Bool   b   -> Bool b
    A.Null       -> Null
#endif
