---
title:                "Deleting characters matching a pattern"
html_title:           "Gleam recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

Are you tired of manually deleting characters that match a specific pattern in your code? Or perhaps you want to clean up messy code and remove unnecessary characters? Look no further, because Gleam has a built-in function that makes deleting characters a breeze!

## How To

To delete characters matching a pattern in Gleam, simply use the `remove_chars` function. This function takes in two arguments: the string you want to modify and the pattern of characters you want to remove.

```
Gleam run "```
let str = "Hello, World!";
let pattern = ", !";
let result = remove_chars(str, pattern);
io.print(result); // Output: "HelloWorld"
```"
```

As you can see, the `remove_chars` function effectively removed the characters `,` and `!` from the original string, leaving us with the desired output of "HelloWorld".

## Deep Dive

The `remove_chars` function uses regular expressions to determine which characters to remove. This allows for more flexibility in specifying the pattern of characters. You can use a single character, a range of characters, or even a more complex expression to match your desired pattern.

For example, if we wanted to remove all numbers from a string, we could use the pattern `[0-9]`, which represents a range of characters from 0 to 9. Or if we wanted to remove all vowels, we could use the pattern `[aeiou]`, which represents a list of characters.

Furthermore, the `remove_chars` function also has an optional third argument, `ignore_case`, which, when set to `true`, will ignore case sensitivity when matching the pattern.

## See Also

To learn more about the `remove_chars` function and other useful string manipulation functions in Gleam, check out the official documentation:  
- [Gleam Documentation](https://gleam.run/documentation)
- [Gleam String module](https://gleam.run/modules/string.html)
- [Regular Expressions Tutorial](https://regexone.com/)