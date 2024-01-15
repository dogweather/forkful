---
title:                "Extracting substrings"
html_title:           "Haskell recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Extracting substrings is a common task in programming, as it allows us to manipulate and extract specific portions of a string. In Haskell, using the right functions and techniques, we can easily extract substrings to perform various operations on them.

## How To

In Haskell, the `take` and `drop` functions are used to extract substrings from a given string. The `take` function takes two arguments, the first being the number of characters to extract and the second being the string itself. Let's see an example of extracting the first three characters from a string:

```Haskell
take 3 "Hello World!"
```

The output of this would be `"Hel"`, as it has extracted the first three characters. Similarly, the `drop` function takes the same arguments but instead of extracting the beginning characters, it drops them and returns the remaining string. Let's try it out:

```Haskell
drop 3 "Hello World!"
```

The output of this would be `"lo World!"`, as it has dropped the first three characters.

We can also use the `substr` function from the `Data.List` library to extract a substring from a specific start index to an end index. Let's take a look at an example:

```Haskell
import Data.List

substr 3 7 "Hello World!"
```

The output of this would be `"lo W"`, as it has extracted the characters from index 3 to 7 (inclusive).

## Deep Dive

In Haskell, we can also use pattern matching and recursion to extract substrings from a given string. Let's consider the following function:

```Haskell
substr :: Int -> Int -> String -> String
substr start end str = if start > end 
                       then [] 
                       else head str : (substr (start+1) end (tail str))
```

This function takes in a start and end index as well as a string, and returns the substring from the start index to the end index. It uses recursion and the `head` and `tail` functions to extract individual characters from the string and add them to a new string.

We can also use the `splitAt` function from the `Data.List` library to split a string at a given index and return the two parts as a tuple. For example:

```Haskell
import Data.List

splitAt 5 "Hello World!"
```

The output of this would be `("Hello", " World!")`, as it splits the string at index 5 and returns the two parts.

## See Also

- [Haskell Docs: take](https://hackage.haskell.org/package/base/docs/Data-List.html#v:take)
- [Haskell Docs: drop](https://hackage.haskell.org/package/base/docs/Data-List.html#v:drop)
- [Haskell Docs: Data.List](https://hackage.haskell.org/package/base/docs/Data-List.html)
- [Haskell Docs: splitAt](https://hackage.haskell.org/package/base/docs/Data-List.html#v:splitAt)

By using the right functions and techniques, extracting substrings in Haskell can be a simple and straightforward task. Remember to check the documentation for more information and utilize pattern matching and recursion for more complex substring extractions. Happy coding!