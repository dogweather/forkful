---
title:                "Elm recipe: Searching and replacing text"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

Searching and replacing text is a fundamental task in programming. It allows you to quickly and efficiently update large amounts of code without having to manually make changes one by one. Using Elm's powerful search and replace functions can save you time and effort in your coding workflow.

## How To

First, import the String module in your code by adding `import String` at the top. This will give you access to various functions for manipulating strings, including `replace` which we will use for search and replace.

```
import String

text = "Hello World! This is a sample text."

newText = String.replace "Hello" "Hi" text
```

In the above code, we are using the `replace` function to replace all instances of "Hello" with "Hi" in the `text` variable. The result is stored in the `newText` variable, which would be "Hi World! This is a sample text." You can use this code in your Elm applications to replace specific words or phrases within strings.

## Deep Dive

The `replace` function in Elm has a few optional parameters that allow for more advanced search and replace operations. For example, you can specify a starting index for the search, or limit the number of replacements made. Additionally, you can use regular expressions in place of simple strings for more complex search patterns.

```
text = "abc123xyzabc123"
regex = Regex.fromString "abc[\\d]+"
newText = String.replace regex "replacement" text
```

In this code, we are using the `Regex` module to perform a search and replace using a regular expression. The result is "replacementxyzreplacement", replacing all instances of "abc" followed by one or more digits with "replacement".

## See Also

- Official Elm Documentation for String Module: https://package.elm-lang.org/packages/elm/core/latest/String
- Learn Regex the Easy Way: https://github.com/zeeshanu/learn-regex
- Online Regex Tester: https://regex101.com/