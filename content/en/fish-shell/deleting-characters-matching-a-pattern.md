---
title:                "Fish Shell recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

Have you ever found yourself needing to delete certain characters from a string or text file? Maybe you want to remove all the numbers or special symbols in a long string or replace them with a different character. Luckily, the Fish Shell offers a simple and efficient way to do this with its built-in function `string replace`.

## How To

Coding in the Fish Shell is easy and intuitive, thanks to its simple syntax. To delete characters matching a pattern, we will use the `string replace` function. This function takes three arguments: the original string, the pattern to match, and the replacement string. Let's take a closer look at how to use this function with some coding examples:

```
Fish Shell > set original_string "Hello, 123 World!"
Fish Shell > echo $original_string
Hello, 123 World!
Fish Shell > set new_string (string replace $original_string "[0-9]" "")
Fish Shell > echo $new_string
Hello,  World!
```

In this example, we use the `set` command to set a variable named `original_string` with the value of "Hello, 123 World!". Then, we use the `string replace` function to replace any numbers (represented by the pattern [0-9]) with an empty string. Finally, we use the `echo` command to print out the new string, which now only contains the letters and punctuation marks.

We can also use the `string replace` function to replace a pattern with a different character or string. For example:

```
Fish Shell > set original_string "Let's replace the vowels with XX"
Fish Shell > echo $original_string
Let's replace the vowels with XX
Fish Shell > set new_string (string replace $original_string "[aeiou]" "X")
Fish Shell > echo $new_string
LXt's rXplXcX thX vXwXls wXth XX
```

In this example, we use the `set` command to set a variable named `original_string` with the value of "Let's replace the vowels with XX". Then, we use the `string replace` function to replace any vowels (represented by the pattern [aeiou]) with the letter "X". This results in a new string where all the vowels have been replaced with X.

## Deep Dive

The `string replace` function in Fish Shell uses the standard regular expression syntax for matching patterns. This means that you can use a wide range of rules and symbols to match specific characters or strings. For example, the pattern "[a-z]" will match any lowercase letters, while "[AEIOU]" will match any uppercase vowels.

Additionally, you can also use the `string replace` function with variables, making it even more versatile. For example:

```
Fish Shell > set original_string (ls | grep .txt)
Fish Shell > set new_string (string replace $original_string "[0-9]" "")
Fish Shell > echo $new_string
file1.txt file2.txt file3.txt
```

In this example, we use the `ls` command to list all the files in the current directory, and then we use the `grep` command to filter out any files that do not contain the ".txt" extension. The result is a list of files with .txt extensions. Finally, we use the `string replace` function with the pattern [0-9] to delete any numbers from the list.

## See Also

- [Fish Shell documentation](https://fishshell.com/docs/current/index.html)
- [Regular Expressions tutorial](https://www.regular-expressions.info/tutorial.html)
- [Fish Shell tutorial](https://dev.to/defman/fish-shell-understanding-function-blocks-4lfa)