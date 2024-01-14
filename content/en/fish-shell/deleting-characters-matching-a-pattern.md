---
title:    "Fish Shell recipe: Deleting characters matching a pattern"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Why

Have you ever found yourself with a text document containing unnecessary or unwanted characters? Perhaps a file filled with unneeded white spaces, or a string with random symbols scattered throughout. These characters can cause errors when running programs or create confusion when trying to read through the document. But fear not, there is a simple solution – deleting characters matching a pattern. 

## How To

To delete characters matching a pattern in Fish Shell, we will be using the `string` command. This command allows us to manipulate strings and is perfect for removing unwanted characters. Let's take a look at an example code below:

```
set my_string "This is a sample string with random symbols like # and %"
string replace -r "#|%|" $my_string ""
```

In the first line, we set the variable `my_string` to our sample string. Then, using the `string replace` command, we specify the `-r` flag to indicate that we want to use a pattern. The pattern we provide is "#|%|" which tells the command to look for any characters that match either the pound sign, percent sign, or pipe symbol. Finally, we provide an empty string as the replacement text, deleting the matched characters from our original string.

The output of this code would be: "This is a sample string with random symbols like  and"

## Deep Dive

The `string replace` command follows a basic syntax of `string replace [options] [pattern] [replacement] [string]`. The `-r` flag we used earlier stands for "regex" and allows us to use regular expressions as our pattern. Regular expressions, or regex, is a powerful tool for matching patterns in text documents. It uses a specific syntax to match characters, numbers, or any other types of characters.

For example, the pattern `[0-9]` would match any numbers in the string, and the pattern `[a-z]` would match any lowercase alphabetic letters. By using these patterns, we can easily target specific types of characters to delete.

It is also important to note that the `string replace` command does not modify the original string, but instead returns a new string. Therefore, if you want to save the changes to the original string, you will need to assign the output of the command to a new or existing variable.

## See Also

- [Fish Shell documentation on string command](https://fishshell.com/docs/current/cmds/string.html)
- [Regular Expressions tutorial](https://www.regular-expressions.info/tutorial.html)
- [Mastering Regular Expressions book](https://www.oreilly.com/library/view/mastering-regular-expressions/0596528124/)