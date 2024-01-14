---
title:                "Haskell recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why
Have you ever found yourself struggling to manually replace text in your code? It can be a time-consuming and tedious task, especially when working with large files. Thankfully, there's a solution - using Haskell to search and replace text. This functional programming language provides a simple and efficient way to automate this process.

## How To
Let's dive into the code and see how we can easily search and replace text using Haskell. First, we need to import the necessary module(s) for our task:

```Haskell
import Data.List
```

Next, we can define a function that takes in two parameters - the old text and the new text, and returns a modified string with the replaced text:

```Haskell
replaceText :: String -> String -> String -> String
replaceText old new str = intercalate new (splitOn old str)
```

In this function, we're using two functions from the `Data.List` module - `splitOn` and `intercalate`. `splitOn` takes in a delimeter (in this case, our old text) and splits the string into a list. We then use `intercalate` to join the list back together, this time with our new text as the separator.

Let's test out our function with some sample text:

```Haskell
replaceText "world" "Haskell" "Hello world"
```

The output of this code would be `"Hello Haskell"`.

But what if we want to replace multiple occurrences of the same text? We can use the built-in `replaceAll` function in the `Data.List.Split` module. Here's an example:

```Haskell
replaceAll "a" "e" "alphabet"
```

The output of this code would be `"elphebet"`.

## Deep Dive
Now that we have a basic understanding of how to search and replace text using Haskell, let's take a deeper look at some other useful functions and methods.

Firstly, we can use the `subRegex` function from the `Text.Regex` module to replace text based on a regular expression pattern. For example:

```Haskell
subRegex (mkRegex "foo[0-9]*") "bar" "Hello foo123"
```

The output of this code would be `"Hello bar"`. Here, we're using the regular expression pattern `foo[0-9]*` to match any text starting with "foo" followed by any number of digits.

Additionally, we can also use the `replace` function from the `Text.Regex.Posix` module to replace text based on a regular expression pattern using POSIX-style regular expressions. Here's an example:

```Haskell
replace "([A-Z])" " $1" "HelloWorld"
```

The output of this code would be `"Hello World"`. The regular expression pattern `([A-Z])` matches any uppercase letter and the replacement string adds a space before the matched letter.

## See Also
- [Haskell Data.List module](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-List.html)
- [Haskell Text.Regex module](https://hackage.haskell.org/package/regex-base-0.93.2/docs/Text-Regex.html)
- [Haskell Text.Regex.Posix module](https://hackage.haskell.org/package/regex-posix-0.96.0.0/docs/Text-Regex-Posix.html)

By using Haskell to search and replace text, we can save time and effort in editing code. And with the range of functions available, we can easily adapt to different cases and patterns. Happy coding!