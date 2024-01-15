---
title:                "Capitalizing a string"
html_title:           "Elm recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

To make words stand out and draw attention, it is often necessary to capitalize them. This is especially important in programming, where capitalization can indicate the start of a new variable or function. In this article, we will explore how to capitalize strings in the functional programming language, Elm.

## How To

To capitalize a string in Elm, we can use the `String.toUpper` function. Let's take a look at an example:

```Elm
String.toUpper "hello world"
```

The output of this code would be `"HELLO WORLD"`. As you can see, the `String.toUpper` function takes a string as its parameter and returns the capitalized version of that string.

But what if we only want to capitalize the first letter of a string, like in a title? Elm has a `String.capitalize` function for that:

```Elm
String.capitalize "elm programming"
```

The output of this code would be `"Elm programming"`. This function only capitalizes the first letter of the first word in the string.

## Deep Dive

In some cases, we may want to capitalize every word in a string, similar to how titles are typically formatted. Elm has a `String.words` function that splits a string into a list of words, and a `String.join` function that joins a list of strings into one string. We can use these functions together to capitalize every word in a string:

```Elm
String.join " " (List.map String.capitalize (String.words "united states of america"))
```

The output of this code would be `"United States Of America"`. Let's break down the code:

- `String.words` takes a string and returns a list of words, in this case `[ "united", "states", "of", "america" ]`
- `List.map` takes a function and a list as its parameters, and applies the function to each element in the list. In this case, we are using the `String.capitalize` function to capitalize each word in the list.
- Finally, `String.join` combines all the words in the list into one string, separated by a space.

By combining these functions, we can create a custom `String.capitalizeAll` function that capitalizes every word in a given string.

## See Also

- [Elm Documentation](https://elm-lang.org/docs)
- [Elm Syntax](https://guide.elm-lang.org)
- [Elm Packages](https://package.elm-lang.org)