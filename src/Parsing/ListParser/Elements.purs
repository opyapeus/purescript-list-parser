module Parsing.ListParser.Elements 
  ( ParseElement(..)
  , ParseElements
  , ElementParser
  , ElementsParser
  , unParseElement
  , eol
  , element
  , oneOf
  , noneOf
  , elements
  , mapure
  )
  where

import Prelude

import Control.Monad.State (gets, modify_)
import Data.List (List, elem, notElem, null, uncons)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Text.Parsing.Parser (ParseState(..), ParserT, fail)
import Text.Parsing.Parser.Combinators (tryRethrow)
import Text.Parsing.Parser.Pos (Position(..))

newtype ParseElement e = ParseElement e

derive instance eqParseElement :: Eq e => Eq (ParseElement e)

instance showParseElement :: Show e => Show (ParseElement e) where
  show (ParseElement e) = "(ParseElement " <> show e <> ")"

type ParseElements e = List (ParseElement e)
type ElementParser m e = ParserT (ParseElements e) m (ParseElement e)
type ElementsParser m e = ParserT (ParseElements e) m (ParseElements e)

unParseElement :: forall e. ParseElement e -> e
unParseElement (ParseElement e) = e

-- | end of the list.
eol :: forall e m. Monad m => ParserT (ParseElements e) m Unit
eol = do
  input <- gets \(ParseState input _ _) -> input
  unless (null input) (fail "Expected EOL")

-- | match any element.
anyElement :: forall e m. Monad m => ElementParser m e
anyElement = do
  input <- gets \(ParseState input _ _) -> input
  case uncons input of
    Nothing -> fail "Unexpected EOL"
    Just { head, tail } -> do
      modify_ \(ParseState _ position _) -> ParseState tail (updatePos position) true
      pure head

-- | match a element satisfying the specified predicate.
satisfy :: forall e m. Show e => Monad m => (ParseElement e -> Boolean) -> ElementParser m e
satisfy f = tryRethrow do
  e <- anyElement
  if f e
    then pure e
    else fail $ "Element " <> show e <> " did not satisfy predicate"

-- | match the specified element.
element :: forall e m. Eq e => Show e => Monad m => ParseElement e -> ElementParser m e
element = satisfy <<< (==)

-- | match one of the elements in the list.
oneOf :: forall e m. Eq e => Show e => Monad m => ParseElements e -> ElementParser m e
oneOf = satisfy <<< flip elem

-- | match one of the elements in the list.
noneOf :: forall e m. Eq e => Show e => Monad m => ParseElements e -> ElementParser m e
noneOf = satisfy <<< flip notElem

-- | match the specified element series.
elements :: forall e m. Eq e => Show e => Monad m => ParseElements e -> ElementsParser m e
elements = traverse element

-- | lift inner type for convenience.
mapure :: forall f a e. Functor f => Applicative a => f e -> f (a e)
mapure = map pure

-- TODO: remove line, column starts with 0
updatePos :: Position -> Position
updatePos (Position { line, column }) = Position { line: line, column: column + 1 }