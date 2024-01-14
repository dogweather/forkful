---
title:                "Haskell recipe: Searching and replacing text"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

In the world of programming, text manipulation is a common task that we encounter on a daily basis. As our programs become more complex, the need for automating this task becomes increasingly important. That's where searching and replacing text comes into play. By using Haskell, a functional programming language, we can easily automate this process and save ourselves time and effort.

## How To

To begin, we first need to import the necessary module, `Text.Regex.Posix`, which contains functions for regular expressions. Next, we can create a regular expression pattern using the `makeRegex` function. This allows us to specify the text we want to search for and any modifiers, such as case sensitivity.

```
import Text.Regex.Posix

-- Example regular expression pattern
let pattern = makeRegex "world" :: Regex
```

Next, we can use the `match` function to find and replace the text in our source string. This function takes in our pattern, the source string and the text we want to replace it with.

```
let source = "Hello world"
let output = match pattern source "universe" :: String
```

The output should now be `Hello universe`, with the original word "world" replaced by "universe". It's important to note that this function will only replace the first occurrence of the pattern. To replace all occurrences, we can use the `replaceAll` function instead.

```
let output = replaceAll pattern source "universe" :: String
```

## Deep Dive

While the `Text.Regex.Posix` module is very handy for simple text replacements, it does have some limitations. For more complex patterns, we can utilize the `Text.Regex.PCRE` module, which supports Perl Compatible Regular Expressions (PCRE).

For example, we may want to replace all digits with an underscore in a source string. This can be achieved using a regular expression pattern like `\\d` which matches all digits. We can then use the `subRegexAll` function to replace all occurrences using this pattern.

```
import Text.Regex.PCRE

let pattern = "[0-9]"
let source = "H3ll0 w0rld"
let output = subRegexAll (mkRegex pattern) source "_" :: String
```

The output will now be `H_ll_ w_rld`, with all digits replaced by underscores. Other useful functions and modifiers can also be found in the `Text.Regex.PCRE` module, allowing for even more complex text replacements.

## See Also

- [Haskell Regex Module](https://hackage.haskell.org/package/regex)
- [Haskell PCRE Module](https://hackage.haskell.org/package/pcre-light)