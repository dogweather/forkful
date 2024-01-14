---
title:    "Haskell recipe: Searching and replacing text"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

Have you ever found yourself in a situation where you needed to change a specific word or phrase in a large block of text? Maybe you have a lengthy document or code file and you need to replace all instances of a certain variable name or function call. Instead of manually going through each line, why not use Haskell to quickly and efficiently search and replace text?

## How To

To start, we'll need to import the `Text.Regex.TDFA` module, which provides regular expression support in Haskell. We'll also use the `sub` function, which allows us to perform a substitution based on a regex pattern and a replacement string.

```Haskell
import Text.Regex.TDFA (sub)
```

Now, let's say we have a string of text that contains multiple instances of the word "cat", but we want to replace them all with "dog". We can use the following code to achieve this:

```Haskell
let newText = sub "cat" "dog" "I have a cat, a black cat, a white cat."
putStrLn newText
```

The output of this code would be:

```
"I have a dog, a black dog, a white dog."
```

We can also use more complex regex patterns to search and replace text. For example, if we want to replace all instances of three consecutive numbers with "123", we can use the following code:

```Haskell
let newText = sub "[0-9]{3}" "123" "My favorite numbers are 123 and 456."
putStrLn newText
```

The output of this code would be:

```
"My favorite numbers are 123 and 123."
```

## Deep Dive

The `sub` function has a few more parameters that allow for more customization. The full signature of the function is `sub :: Regex -> String -> ByteString -> ByteString`, where the first parameter is the regex pattern, the second parameter is the replacement string, and the third parameter is the input text.

The function also has an optional fourth parameter, called `Subst`, which allows for more options in the replacement process. For example, we can use the `SubAll` option to replace all instances of the pattern, or `SubOnce` to only replace the first instance. There are also options for case sensitivity and multiline matching.

Furthermore, the `Regex` type itself has many features and functions to help us create complex and efficient regex patterns. With the combination of `Regex` and `sub`, we have a powerful tool for searching and replacing text in Haskell.

## See Also

- [Haskell documentation on Text.Regex.TDFA](https://hackage.haskell.org/package/regex-tdfa/docs/Text-Regex-TDFA.html)
- [Tutorial on regular expressions in Haskell](https://www.haskell.org/tutorial/patterns.html)
- [Examples of regex patterns in Haskell](https://wiki.haskell.org/Regular_expressions)