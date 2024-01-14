---
title:    "Elm recipe: Using regular expressions"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why Regular Expressions are Useful in Elm Programming

Regular expressions are an essential tool for any programmer, and they are especially useful in Elm. They allow developers to efficiently search and manipulate strings, making data processing and validation much more manageable. If you want to become a proficient Elm programmer, learning how to use regular expressions is a must.

## How To Use Regular Expressions in Elm

Using regular expressions in Elm is straightforward and intuitive. First, you need to import the `Regex` module from the core library:

```Elm
import Regex
```

Next, you can use the `Regex.find` function to search for patterns within a string. Here's an example that checks if a given string contains a valid email address:

```Elm
validEmail : Regex.Pattern
validEmail =
    Regex.regex "[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"

regexExample : Bool
regexExample =
    Regex.find validEmail "example@email.com"
```

In this example, `validEmail` is a regular expression pattern that checks for a valid email address format. Then, the `Regex.find` function searches for this pattern within the given string and returns a `Bool` value (in this case, `True`).

## Deep Dive into Regular Expressions

Regular expressions in Elm follow the same syntax and conventions as regular expressions in other languages. However, there are a few differences to keep in mind when using them in Elm. 

One of the main differences is that Elm's regular expression engine is purely functional, which means that it operates without any side effects. Another noteworthy difference is that Elm's regular expressions are Unicode-aware by default, making them more powerful and versatile.

The Elm community has created a useful library called `elm-regex-extras` that provides additional functionalities such as sub-matches and lookarounds. It's worth checking out if you want to explore more advanced regular expression techniques in Elm.

## See Also

- Official Elm documentation on Regular Expressions: https://package.elm-lang.org/packages/elm/regex/latest/
- `elm-regex-extras` library: https://package.elm-lang.org/packages/jamesmacaulay/elm-regex-extras/latest/Regex-Extras
- Regular Expressions 101 (an interactive regular expression tester): https://regex101.com/