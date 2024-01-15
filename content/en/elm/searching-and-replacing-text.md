---
title:                "Searching and replacing text"
html_title:           "Elm recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

Have you ever found yourself in the tedious task of manually searching and replacing a particular word or phrase in a large chunk of text? Maybe it's a typo or a repeated word that needs to be fixed. This is where the power of searching and replacing text comes in, making it a valuable tool for any developer or writer.

## How To

Let's dive into some Elm code to see how we can efficiently perform a search and replace task.

First, we need to import the `String` module, which contains the `replace` function that allows us to specify the search term and the replacement text. We also need to import the `Array` module because we will be working with arrays of strings.

```
import String exposing (replace)
import Array exposing (singleton)
```

Next, we can define our source text as an array of strings:

```
sourceText = ["Hello", "world", "this", "is", "a", "test", "sentence"]
```

Now, let's say we want to replace the word "test" with "sample". We can use the `replace` function to specify the search term and the replacement text, and pass in our source text as the third argument.

```
replacedText = replace "test" "sample" sourceText
```

The `replacedText` variable will now contain the updated array of strings with the search term replaced by the replacement text. We can use `String.join` to combine all the strings into one final string.

```
finalText = String.join " " replacedText
```

If we print out the `finalText` variable, it will output: "Hello world this is a sample sentence". 

You can also use regular expressions as the search term, allowing for more complex and dynamic replacements. For example, if we want to replace all instances of numbers in a string with the word "number", we can use the following code:

```
input = "I have 20 apples and 3 bananas"
result = replaceRegex "\\d+" "number" input
```

The `result` variable will now contain the string: "I have number apples and number bananas".

## Deep Dive

Underneath the hood, the `replace` function uses the `replaceRegex` function, which allows for more advanced replacements using regular expressions. This means that any regular expression that is valid in JavaScript regex will also work in Elm.

The `replace` function also handles case sensitivity by default, but you can use the `replaceInsensitive` function to ignore case when performing replacements.

## See Also

- Official Elm documentation on searching and replacing text: https://package.elm-lang.org/packages/elm-lang/core/latest/String#replace
- Useful regular expressions for text replacement: https://www.regular-expressions.info/examples.html