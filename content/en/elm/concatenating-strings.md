---
title:                "Concatenating strings"
html_title:           "Elm recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?

Concatenating strings is the process of combining multiple strings into one, creating a longer string. Programmers use it to manipulate and construct strings in a specific format for various purposes, such as creating output or formatting data. It is a fundamental concept in programming and is widely used in many programming languages.

## How to:

In Elm, string concatenation is achieved by using the `++` operator. This operator takes two strings and combines them into one. Let's see an example:

```Elm
"Hello" ++ " " ++ "World" -- Output: Hello World
```

We can also concatenate variables that hold strings:

```Elm
let name = "Alice"
let greeting = "Hello "
greeting ++ name -- Output: Hello Alice
```

## Deep Dive

String concatenation has been around for a long time and is a basic operation in computer science. In the early days of computing, strings were often manipulated by directly modifying the underlying memory, which was a complex and error-prone process. With modern programming languages like Elm, string concatenation has become much more straightforward and intuitive.

In some programming languages, there are other ways to concatenate strings, such as using the `+` operator or the `concat()` function. However, in Elm, the `++` operator is the only way to concatenate strings.

One important detail to note is that string concatenation can be expensive, especially when done on a large scale. This is because every time strings are concatenated, a new string must be created. In cases where performance is critical, it is better to use alternative methods, such as using a string builder or formatting strings before concatenating them.

## See Also

To learn more about string concatenation in Elm, check out the official Elm documentation: https://package.elm-lang.org/packages/elm/core/latest/String#++