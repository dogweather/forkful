---
title:    "Fish Shell recipe: Extracting substrings"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Why

Substrings, or smaller pieces of strings within a larger string, are a common occurrence in programming. Extracting substrings can be a useful skill in various scenarios, such as manipulating text or parsing data. In this blog post, we will explore how to extract substrings using the Fish Shell and why it’s a valuable tool to have in your programming arsenal.

## How To

The Fish Shell provides a simple and efficient way to extract substrings using the `string` command. Let’s take a look at some coding examples and their corresponding outputs.

```
# Extract a substring starting from index 2 with a length of 5
string substr "fish shell" 2 5 
# Output: ish s

# Extract a substring starting from the end with a length of 3
string substr "fish shell" -3
# Output: ell
```

The first line of code starts extracting from index 2, which is the letter “i” in “fish shell”, and the length of 5 includes the characters starting from “ish” and ending at “s”. Similarly, the second line of code starts extracting from the end, with a starting index of -3, which includes the last 3 characters of “fish shell”.

We can also use variables to store the extracted substring instead of immediately displaying it on the terminal. For example:

```
set original_string "fish shell"
set substring (string substr $original_string 1 4)
echo $substring
# Output: fish
```

In the above code, the variable `original_string` holds the value “fish shell”. Then, we use the `string` command to extract a 4-character substring starting from index 1, which is “fish”. The extracted substring is then stored in the variable `substring`, and the `echo` command displays it on the terminal.

## Deep Dive

The `string` command has more options for extracting substrings, such as using regular expressions or specifying a delimiter. You can use these options to extract more complex substrings based on specific patterns or conditions.

For example, let’s say we have a string that contains a name and an email address separated by a comma. We can use the `string` command and a delimiter to extract only the email address from the string.

```
set original_string "John Doe, john.doe@example.com"
set delimiter ","
set email (string split $original_string $delimiter -1)
echo $email
# Output: john.doe@example.com
```

In the above code, we use the `string split` command to separate the string at the comma delimiter. The `-1` option tells the command to only keep the last part of the split string, which is the email address.

## See Also

- [Fish Shell official documentation](https://fishshell.com/docs/current/cmds/string.html)
- [Fish Shell tutorial on string manipulation](https://fishshell.com/docs/current/tutorial.html#string-manipulation)
- [Regular expressions in Fish Shell](https://fishshell.com/docs/current/index.html#pattern-matching)

Extracting substrings using the Fish Shell can be a powerful tool in your programming toolkit. It allows for efficient text manipulation and parsing, making your code more dynamic and versatile. So go ahead and practice extracting substrings in your projects, and see how it can elevate your programming game. Happy coding!