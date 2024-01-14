---
title:    "Elm recipe: Searching and replacing text"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Why Search and Replace Text in Elm?

Searching and replacing text is a common task in programming. It allows you to quickly make changes to your code without having to manually go through each line. In this blog post, we will explore how to perform search and replace operations in Elm, a functional programming language.

# How To Search and Replace Text in Elm

To search and replace text in Elm, we will use the `String.replace` function. This function takes in three arguments: the text to search for, the replacement text, and the original string.

```
Elm String.replace
```

Let's say we have a string called `greeting` with the value "Hello World". If we want to replace "Hello" with "Hi", we can do so using the following code:

```
Elm
greeting = "Hello World"
greeting = String.replace "Hello" "Hi" greeting
```

The output of `greeting` will now be "Hi World".

We can also use regular expressions to search for patterns in our text. For example, if we want to replace all numbers in a string with the word "number", we can use this code:

```
Elm
message = "I have 10 apples and 5 oranges"
message = String.replace Regex.digit "number" message
```

The output of `message` will now be "I have number apples and number oranges".

# Deep Dive into String.replace

The `String.replace` function is part of the `String` module in Elm. It supports regular expressions, making it a powerful tool for searching and replacing text. The first argument of the function can be either a `String` or a `Regex` value. If it is a `String`, it will search for an exact match in the original string. If it is a `Regex`, it will search for a pattern in the original string.

Another useful function in the `String` module is `String.replaceOnce`. This function works the same as `String.replace`, but it only replaces the first occurrence of the search string or pattern.

# See Also
- Official Elm Documentation: https://package.elm-lang.org/packages/elm/core/latest/String#replace
- Regular Expressions in Elm: https://elmprogramming.com/elm-regex.html
- How to Manipulate Strings in Elm: https://dev.to/elmprogramming/how-to-manipulate-strings-in-elm-5a35