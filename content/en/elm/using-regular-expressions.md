---
title:                "Using regular expressions"
html_title:           "Elm recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?

Regular expressions are a powerful tool used by programmers to find and manipulate patterns in text. These patterns can include specific words, numbers, or symbols, making them a crucial part of data validation and text processing. 

## How to:

To use regular expressions in Elm, first, we need to import the Regex module using the ```import Regex``` statement. Then, we can use the ```Regex.find``` function to search for a specific pattern in a given string. For example, to find all words that start with the letter "a" in a sentence, we can use the following code:

```Elm
import Regex

sentence = "Apples are always delicious."

Regex.find (Regex.regex "a\\w+") sentence

```

The output would be a list of matches, in this case, ["Apples", "always"].

We can also use regular expressions for more complex patterns, such as email validation. To validate an email address in Elm, we can use the following code:

```Elm
import Regex

email = "info@example.com"

Regex.find (Regex.regex "[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,4}") email
```

The output would be ["info@example.com"] if the email is valid, or an empty list if it is not.

## Deep Dive:

Regular expressions have been around since the 1950s and are used in many programming languages, including Elm. They offer a concise and efficient way to find and manipulate text patterns. 

While regular expressions can be powerful, they can also be complex and challenging to understand. For simpler text patterns, Elm offers alternative functions such as ```String.startsWith``` and ```String.contains```, which provide an easier way to find specific strings within a given text.

In terms of implementation, regular expressions in Elm are based on the PCRE library and support a wide range of features, including grouping, quantifiers, and character classes. The official Elm website offers detailed documentation on Regex functions and their usage.

## See Also:

To learn more about regular expressions in Elm, check out the official Regex documentation at <https://package.elm-lang.org/packages/elm/regex/latest/>. You can also explore other helpful resources, such as online tutorials and forums, to deepen your understanding of this powerful text processing tool.