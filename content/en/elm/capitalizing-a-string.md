---
title:                "Elm recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

Capitalizing strings may seem like a simple task, but it is a common requirement in programming. Whether you are creating a form for user input or formatting text for display, capitalization can greatly enhance the readability and aesthetics of your code. In this post, we will explore how to capitalize strings in Elm and why it is an important skill for any programmer to have.

## How To

In Elm, there are a few different ways to capitalize a string depending on your specific needs. Let's take a look at some examples using the `String` module:

```
-- Capitalize the first letter of a string
String.capitalize "elm" --> "Elm"

-- Capitalize all letters in a string
String.toUpper "elm" --> "ELM"
```

As you can see, the `String.capitalize` and `String.toUpper` functions are the main tools for capitalization in Elm. These functions work on Unicode characters, making them suitable for languages with non-Latin characters.

If you want to capitalize only the first letter of each word in a string, you can use the `String.words` and `List.map` functions in combination with `String.capitalize`.

```
-- Capitalize first letter of each word in a string
words = String.words "elm programming is awesome!"
List.map String.capitalize words --> ["Elm", "Programming", "Is", "Awesome!"]
```

Another useful technique for capitalization is pattern matching. This allows you to create custom rules for capitalizing strings based on certain criteria.

```
-- Capitalize string based on length
firstLetter = \word ->
    String.left 1 word
        |> String.toUpper

otherLetters = \word ->
    String.dropLeft 1 word

capitalizedString = \word ->
    case String.length word of
        0 -> ""
        1 -> word
        _ -> firstLetter word ++ otherLetters word

capitalizedString "elm" --> "Elm"
capitalizedString "Programming" --> "Programming"
```

## Deep Dive

In addition to the `String` module, there is also the `Text` module in Elm which provides more advanced features for capitalization. The `Text` module uses a type-safe approach to working with text, ensuring that your code won't compile if you try to perform invalid operations.

One useful function in the `Text` module is `Text.toTitle`. This takes a string and capitalizes the first letter of each word, while converting the rest of the letters to lowercase.

```
import Text exposing (toTitle)

toTitle "elm programming" --> "Elm Programming"
```

Another helpful function in the `Text` module is `Text.toUpperFirst`. This allows you to capitalize only the first letter of a string, while keeping the rest of the letters in lowercase.

```
toUpperFirst "elm" --> "Elm"
toUpperFirst "PrograMMIng" --> "Programming"
```

## See Also

Now that you know how to capitalize strings in Elm, here are some additional resources to continue your learning journey:

- [Official Elm documentation on the `String` module](https://package.elm-lang.org/packages/elm/string/latest/)
- [Official Elm documentation on the `Text` module](https://package.elm-lang.org/packages/elm/text/latest/)
- [Elm in Action book](https://www.manning.com/books/elm-in-action)
- [Elm community website for more helpful resources](https://elm-lang.org/community)