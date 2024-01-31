---
title:                "Extracting substrings"
date:                  2024-01-20T17:45:34.263238-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extracting substrings"

category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
Extracting substrings means pulling specific parts out of a string. Programmers do it to isolate, manipulate, or analyze pieces of text data.

## How to:

Elm makes it easy. For a start, let's use `String.slice`:

```Elm
import String exposing (slice)

fullText : String
fullText = "Hello, Elm world!"

-- Extracting "Elm"
substring : String
substring = slice 7 10 fullText

-- Output: "Elm"
```

Now, let's get a bit more dynamic with `String.left` and `String.right`:

```Elm
import String exposing (left, right)

-- Getting first 5 characters
leftString : String
leftString = left 5 fullText

-- Output: "Hello"

-- Getting last 5 characters
rightString : String
rightString = right 5 fullText

-- Output: "orld!"
```

## Deep Dive

Historically, substring extraction is as old as programming itself. In Elm, as in other functional languages, string manipulation functions are immutable - they return new strings rather than altering the original.

Alternatives like `String.dropLeft` and `String.dropRight` exist. They trim characters from either end of the string:

```Elm
import String exposing (dropLeft, dropRight)

-- Dropping first 7 characters
droppedLeftString : String
droppedLeftString = dropLeft 7 fullText

-- Output: "Elm world!"

-- Dropping last 6 characters
droppedRightString : String
droppedRightString = dropRight 6 fullText

-- Output: "Hello, Elm"
```

Implementation-wise, these functions are built into the Elm standard library and handle Unicode, though there are considerations to be made with Unicode's surrogate pairs and combining characters.

## See Also

- Elm `String` module documentation: https://package.elm-lang.org/packages/elm/core/latest/String
- Elm guide on strings: https://guide.elm-lang.org/strings/
- MDN Web Docs on Unicode: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/charAt
