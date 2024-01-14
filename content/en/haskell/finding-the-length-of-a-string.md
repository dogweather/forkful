---
title:                "Haskell recipe: Finding the length of a string"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

As programmers, we often need to manipulate strings in our code. One common task is finding the length of a string. In Haskell, this can be achieved using a simple function, but it's also important to understand the underlying concepts behind it.

## How To

```Haskell
-- Define a function to find the length of a string
len :: String -> Int
len str = length str

-- Call the function with a string as an argument
len "Hello World"
```

Output: `11`

The `len` function takes in a string as an argument and uses the `length` function to return the number of characters in the string. This can be done with any string, not just with the example "Hello World."

Here's another example using the `len` function with an empty string:

```Haskell
len ""
```

Output: `0`

This result makes sense because an empty string has no characters, therefore its length is 0.

## Deep Dive

Now, let's take a deeper look at how the `length` function actually works. In Haskell, every string is represented as a list of characters, so finding the length of a string is essentially finding the length of a list.

```Haskell
length "banana"
```

Output: `6`

The `length` function recursively counts the number of elements in a list until it reaches the end, denoted by an empty list `[]`. For example, the above function call would be evaluated as follows:

`length "banana"` = `1 + length "anana"` = `1 + 1 + length "nana"` = `1 + 1 + 1 + length "ana"` = `1 + 1 + 1 + 1 + length "na"` = `1 + 1 + 1 + 1 + 1 + length "a"` = `1 + 1 + 1 + 1 + 1 + 1 + length ""` = `1 + 1 + 1 + 1 + 1 + 1 + 0` = `6`

As you can see, the `length` function keeps adding 1 for each element in the list until it reaches the end and returns the final count.

## See Also

To learn more about strings and string manipulation in Haskell, check out these resources:

- [Haskell Strings](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-String.html)
- [A Beginner's Guide to Haskell: Strings](https://www.fpcomplete.com/haskell/tutorial/haskell-strings)
- [Haskell Tutorial: String and IO](https://hackernoon.com/haskell-tutorial-strings-and-io-5ab7ea91a4e8)