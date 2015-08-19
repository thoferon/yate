# What is Yate?

Yate stands for Yet Anoter Template Engine and is a Haskell library. It is a
agnostic meaning it doesn't know anything about any particular output format.
It is therefore not suitable for everything, e.g. XSS issues in HTML files.

# Example

*NB: We'll use JSON herefor the sake of simplicity, but you can use your own
data structures by instanciating the typeclass `ToYate`.*

## template.txt

```
Items owned by {%= author.name %}:
{% forall items %}
* {%= .description %}{% if .value %} ({%= .value %}){%end}
{% end %}
```

## data.json

```
{
  "author": {
    "name": "Tom"
  },
  "items": [
    {
      "description": "Item 1",
      "value": 11.99
    },
    {
      "description": "Item 2"
    }
  ]
}
```

## Main.hs

{-# LANGUAGE TemplateHaskell #-}

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL

import           Text.Yate

template :: Template Value
template = $(loadTemplateFile "template.txt")

main :: IO ()
main = do
  jsonContent <- BSL.readFile "data.json"
  Just  dat   <- decode jsonContent
  Right out   <- renderTemplate template dat
  putStrLn out

## Output

Items owned by Tom:
* Item 1 (11.99)
* Item 2

# Syntax

## Paths

Absolute paths are of the form `a.b.c` and describe the path to traverse in the
data structure to get a certain element. Relative paths have the same function
but start from the current object (see `in` and `forall`.)

## Variable insertion

This is as simple as `{%= PATH %}`.

## If statement

```
{% if PATH %}
  Block 1
{% else %}
  Block 2
{% end %}
```

or

```
{% if PATH %}
  Block
{% end %}
```

## For statement

```
{% for NAME in PATH %}
  Block
{% end %}
```

A variable called NAME is added to the main object in the evaluation of the
block. Thus, it is accessible with absolute paths.

## In statement

```
{% in PATH %}
  Block
{% end %}
```

The current object for relative paths is changed to the one accessible with
the given path.

## Forall statement

```
{% forall PATH %}
  Block
{% end %}
```

is some syntactic sugar for:

```
{% for _element in PATH %}{% in _element %}
  Block
{% end %}{% end %}
```

## Note about variable names

Characters allowed in names are alphanumeric characters as well as
`-~!@#$%^&*_+=;:'?`.

# API Documentation

See https://hackage.haskell.org/package/yate for more information.