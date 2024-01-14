---
title:    "Elm recipe: Searching and replacing text"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Why

When working with code, it's inevitable that you will come across situations where you need to make changes to multiple lines of text at once. This is where the search and replace function comes in handy. Instead of manually going through each line, search and replace allows you to quickly and efficiently make changes to your entire codebase.

## How To

To use the search and replace function in Elm, you will need to import the Text module. This module provides functions for manipulating strings, including the `replace` function. Let's take a look at how we can use this function to search and replace text within a string.

First, we need to define a string with the text we want to make changes to. For this example, let's say we have the following string:

```
let text = "Hello, world! This is a test string."
```

Next, we can use the `replace` function to replace the word "test" with "new" in our string. The `replace` function takes in three arguments: the string we want to search within, the text we want to replace, and the new text we want to replace it with. In this case, it would look like this:

```
let newText = replace "test" "new" text
```

The value of `newText` would now be "Hello, world! This is a new string." This simple example shows how easy it is to use the search and replace function in Elm to make changes to text.

## Deep Dive

The `replace` function in the Text module is just one way to search and replace text in Elm. There are also other functions such as `replaceAll`, which replaces all instances of a given substring, and `replaceRegex`, which allows you to use regular expressions for more complex replacements.

Additionally, the Text module also provides functions for searching and replacing text within a specific index or range of the string. This gives you even more flexibility in making changes to your code.

## See Also

- [Elm Text module documentation](https://package.elm-lang.org/packages/elm/core/latest/Text)
- [Regular expressions in Elm](https://package.elm-lang.org/packages/elm/regex/latest/)
- [Free Elm tutorial for beginners](https://frontendmasters.com/courses/beginner-elm/)