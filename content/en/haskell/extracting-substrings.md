---
title:    "Haskell recipe: Extracting substrings"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Substring extraction is a useful technique in programming, especially when working with text. It allows us to extract a specific portion of a string, which can be useful in a variety of applications. Whether you're manipulating user input or parsing through large text files, understanding how to extract substrings can greatly enhance your coding skills.

## How To

Extracting substrings in Haskell is done using the `take` and `drop` functions. These functions take in a string and a number representing the desired length of the substring. The `take` function returns the specified number of characters from the beginning of the string, while the `drop` function returns the remaining characters after the specified number. Let's see an example below:

```Haskell
-- Extract the first 5 characters from the string "Hello World"
take 5 "Hello World" 
-- Output: "Hello"

-- Return the remaining characters after the first 5 from the string "Hello World"
drop 5 "Hello World"
-- Output: " World"
```

We can also use the `splitAt` function to split a string at a specific index, returning a tuple of the substrings before and after the split point.

```Haskell
-- Split the string "Hello World" at index 5
splitAt 5 "Hello World"
-- Output: ("Hello", " World")
```

These functions can also be used in conjunction with other functions, such as `length` and `words`, to create more dynamic substring extraction. For example, if we wanted to extract the first word of a sentence, we could use the `length` and `take` functions to do so.

```Haskell
-- Extract the first word from the sentence "This is a test"
take (length (words "This is a test") !! 0) "This is a test"
-- Output: "This"
```

## Deep Dive

Behind the scenes, these functions are using the `substring` function from the standard library. This function takes in a string, a starting index, and a length, and returns the specified substring. Understanding this can be helpful in troubleshooting any issues with your substring extraction code.

It's also worth noting that these functions operate on the entire string, including any white space. If you want to extract a substring without any leading or trailing white space, you can use the `trim` function from the `Data.Text` library before using the `take` and `drop` functions.

## See Also

- [Haskell String Functions](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-String.html)
- [Haskell Text Functions](https://hackage.haskell.org/package/text-1.2.4.0/docs/Data-Text.html)
- [Haskell List Functions](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html)