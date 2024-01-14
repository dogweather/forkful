---
title:    "Fish Shell recipe: Capitalizing a string"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Why

Capitalizing a string may seem like a simple task, but it can be important for data consistency and presentation purposes. In Fish Shell programming, it is a commonly used operation to ensure that string data is formatted correctly.

## How To

To capitalize a string in Fish Shell, we will be using the `sed` command and regular expressions. `sed` stands for "stream editor" and is a powerful tool for manipulating text. With the use of regular expressions, we can specify patterns to search for and replace in a given string.

Let's take a look at a basic example of capitalizing a string. In this case, we will capitalize the first letter of each word in the string "hello world".

```
Fish Shell:
echo "hello world" | sed -e "s/\b\(.\)/\u\1/g"
```

The output of this code would be "Hello World". Let's break down the code to understand what is happening.

- `echo "hello world"` prints the string "hello world" to the terminal.
- `|` is referred to as a pipe and involves sending the output of the previous command as the input to the next command.
- `sed` is the command we are using for finding and replacing text.
- `-e` specifies the command that we want to execute.
- `"s/\b\(.\)/\u\1/g"` is the regular expression that we will use for capitalization.
    - `s` stands for "substitute" and indicates that we want to replace something.
    - `\b` matches the beginning of a word.
    - `\(.\)` captures the first letter of each word.
    - `\u` specifies that we want to capitalize the first letter.
    - `\1` refers to the first captured group.
    - `g` stands for "global" and ensures that all instances of the pattern are replaced.

This example only covers capitalizing the first letter of each word, but you can adjust the regular expression to suit your needs. For example, if you want to capitalize the entire string, you can use `sed -e "s/\(.*\)/\u\1/g"`.

## Deep Dive

For a deeper understanding of the `sed` command and regular expressions, check out the official documentation. Additionally, there are plenty of online resources and tutorials available for mastering regular expressions.

Using the `sed` command for capitalizing strings is just one application of regular expressions in Fish Shell programming. Practicing and experimenting with different patterns and commands can help you become more proficient in manipulating text and data in your code.

## See Also

- [Official `sed` documentation](https://www.gnu.org/savannah-checkouts/gnu/sed/manual/sed.html)
- [Regular Expression tutorial](https://regexone.com/)
- [Fish Shell documentation](https://fishshell.com/docs/current/)