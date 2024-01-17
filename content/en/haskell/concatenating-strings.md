---
title:                "Concatenating strings"
html_title:           "Haskell recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?
Concatenating strings in Haskell is the process of combining multiple strings into one longer string. Programmers often use this technique when they need to create longer, more complex strings by piecing together smaller strings. It can also be used for formatting purposes, such as adding spaces or punctuation in between strings.

## How to:
To concatenate strings in Haskell, you can use the `++` operator. Here is an example of using `++` to combine the strings "Hello" and "World" into one string:

```Haskell
"Haskell" ++ " " ++ "is" ++ " " ++ "fun!" 
```

This would output the following:

```Haskell
"Haskell is fun!"
```
You can also use the `concat` function to concatenate a list of strings. Here is an example using the `words` function to split a sentence into a list of words, and then using `concat` to combine the words back into a sentence:

```Haskell
concat (words "Let's learn Haskell together!")
```

This would output:

```Haskell
"Let's learn Haskell together!"
```

## Deep Dive:
Concatenating strings has been a common practice in programming languages since the early days of computer programming. It is a simple and efficient way to manipulate and construct strings. In Haskell, strings are represented as lists of characters, so the `++` operator works by combining two lists of characters into one.

Alternatives to using the `++` operator or the `concat` function include using the `concatMap` function or using the `foldl` function. These functions are more advanced and may be necessary for more complex string concatenation tasks.

## See Also:
To learn more about concatenating strings in Haskell, check out the [official documentation](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-List.html#g:22) for the `++` operator and the [Haskell Wiki](https://wiki.haskell.org/Strings_and_characters) page on strings and characters. You can also explore other functions related to string manipulation such as `map` and `filter`. Keep practicing and experimenting with different methods of string concatenation to become more proficient in Haskell programming.