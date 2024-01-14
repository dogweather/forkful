---
title:                "Gleam recipe: Using regular expressions"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

Regular expressions are a powerful tool for string pattern matching and manipulation. They allow for efficient and accurate searching and replacing of text in a given string. As a Gleam programmer, understanding and utilizing regular expressions can greatly enhance your coding abilities and make your programs more versatile.

## How To

To use regular expressions in Gleam, we first need to import the `regex` package. This package provides the necessary functions and types for working with regular expressions. We can do this by adding the following line to our Gleam code:

```Gleam
import regex
```

Next, we need to construct a regular expression using the `regex.compile` function. This function takes a string as its argument, which is the pattern we want to match. For example, if we want to find all words that start with the letter "c" in a given string, our pattern would be `^c\w*`. This will match any character that starts with the letter "c" and is followed by any number of alphanumeric characters.

Now, we can use the regular expression we created with the `regex.matches` function. This function takes two arguments: the regular expression we want to match and the string we want to search. It returns a list of matches, which we can then process in our code. Here is an example of how we could use regular expressions to find all words starting with "c" and print them out:

```Gleam
import regex

let pattern = regex.compile("^c\\w*")
let string = "Coding is cool and creative!"

let matches = regex.matches(pattern, string) // ["Coding" "cool" "creative"]
for match in matches {
  io.println(match)
}
```

Running this code would output:

```
Coding
cool
creative
```

## Deep Dive

Regular expressions offer even more functionality and power when combined with other built-in functions in Gleam. For example, we can use the `regex.replace` function to substitute certain patterns with new text. We can also use the `regex.split` function to split a string into a list of substrings based on a specified pattern.

Regular expressions also have various special characters and modifiers that allow for more complex and specific pattern matching. Some common ones include `+` for one or more occurrences, `?` for zero or one occurrence, and `*` for zero or more occurrences.

For more in-depth information on regular expressions and their usage in Gleam, refer to the official Gleam documentation or external resources listed in the "See Also" section below.

## See Also
- [Gleam documentation on regular expressions](https://gleam.run/book/tour/regular_expressions.html)
- [Regexone - learn regular expressions interactively](https://regexone.com/)
- [Regex cheat sheet](https://www.rexegg.com/regex-quickstart.html)