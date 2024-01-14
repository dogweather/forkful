---
title:                "Gleam recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

Deleting characters matching a pattern can be a useful task when working with text data. It allows for easy manipulation of strings and can save time and effort when cleaning up data.

## How To

To delete characters matching a pattern in Gleam, we can use the `replace` function from the `String` module. This function takes in a string, pattern, and replacement string as arguments. Here's an example of how we can use it:

```
String.replace("Hello, World!", ",", "")
```

This code will return the string "Hello World!" as the `,` character is deleted from the original string. We can also use regular expressions as our pattern to match and delete certain characters. For example:

```
String.replace_regex("Hello, World!", "[^a-zA-Z0-9 ]", "")
```

This code will remove all characters except for letters, numbers, and spaces from the string, resulting in "Hello World" as the output.

## Deep Dive

When working with the `replace` function, it's important to keep in mind that it creates a new string instead of modifying the original one. This means that we need to assign the result of the `replace` function to a new variable if we want to use it later in our code.

Additionally, using regular expressions as our pattern opens up a wide range of possibilities for matching and deleting characters. We can use special characters and quantifiers in our regular expression to make it more specific, such as `*` to match any number of a certain character or `+` to match one or more of a certain character.

## See Also

- Official Gleam documentation on `String.replace`: https://gleam.run/docs/stdlib/string#replace
- Regular Expressions tutorial: https://regexone.com/