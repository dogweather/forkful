---
title:                "Capitalizing a string"
html_title:           "Fish Shell recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Capitalizing a string simply means converting the first letter of each word in the string to uppercase. This is commonly done in programming to ensure consistency and readability in text. It also makes the code more user-friendly and easier to understand.

## How to:

```fish
set my_string "hello world"
echo (string capitalize $my_string) # Output: Hello World
```

In the above example, we first set a variable named `my_string` with the value "hello world". Then we use the `string capitalize` command to convert the first letter of each word to uppercase and finally we use the `echo` command to print the output.

## Deep Dive:

Capitalizing strings has been a common practice since the early days of programming when computers were not case-sensitive. If you are working with a programming language that is not case-sensitive, capitalizing your string can help differentiate between variables and reserved keywords.

Some alternative ways to capitalize a string include using regular expressions or writing a custom function to loop through the string and convert each first letter to uppercase. However, most modern programming languages now have built-in functions or methods to easily capitalize strings.

In the Fish Shell, the `string capitalize` command uses the `strtoupper()` function from the C language to capitalize strings. This function is also used in other programming languages like PHP and Java.

## See Also:

- [Fish Shell documentation for string commands](https://fishshell.com/docs/current/commands.html#string)
- [Understanding String Capitalization in Programming](https://www.geeksforgeeks.org/understanding-string-capitalization/)
- [strtoupper() function documentation](https://www.php.net/manual/en/function.strtoupper.php)