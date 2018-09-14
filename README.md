# purescript-list-parser

A list element pareser which diverted [purescript-parsing](https://github.com/purescript-contrib/purescript-parsing)

Motivation: a parser library for list elements could not be found.

※ It may not be a good solution.

## Example

imports

```PureScript
> import Parsing.ListParser.Elements
> import Text.Parsing.Parser
> import Data.List
```

element, elements

```PureScript
> runParser (map ParseElement $ fromFoldable [1, 2, 3]) (element (ParseElement 1))
(Right (ParseElement 1))
> runParser (map ParseElement $ fromFoldable [1, 2, 3]) (elements (map ParseElement $ fromFoldable [1, 2]))
(Right ((ParseElement 1) : (ParseElement 2) : Nil))
```

combinators can also be used.
