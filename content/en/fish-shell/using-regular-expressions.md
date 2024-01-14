---
title:    "Fish Shell recipe: Using regular expressions"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

Regular expressions, also known as regex, are powerful tools used in text manipulation and search operations. They allow for more efficient and targeted manipulation of text, making it a useful skill for programmers and data analysts.

## How To

Using regular expressions in the Fish Shell is very straightforward. Here's an example of how to search for a specific pattern in a text file and output the results:

```Fish Shell
grep 'apple' fruits.txt
```

This code block uses the `grep` command, which stands for "global regular expression print". It searches the file `fruits.txt` for the pattern "apple" and prints out all the lines that contain that pattern. 

Regular expressions also allow for more complex searches using special characters and modifiers. Here's an example of searching for all words that start with the letter "a":

```Fish Shell
grep -w '\<a\w*' words.txt
```

The `-w` option ensures that only whole words starting with "a" are returned and the `\<` symbol marks the beginning of a word. This example also uses the wildcard character `*` to signify any number of characters following the letter "a". 

Additionally, regular expressions can be used for replacing text using the `sed` command. Here's an example of replacing all instances of "cat" with "dog" in a file called `animals.txt`:

```Fish Shell
sed 's/cat/dog/g' animals.txt
```

The `s/` indicates the substitution operation, and the `g` modifier means to replace all instances, not just the first one.

## Deep Dive

Regular expressions have a rich syntax that allows for even more specific and complex pattern matching. Here are some commonly used symbols and modifiers:

- `^` - signifies the beginning of a line
- `$` - signifies the end of a line
- `.` - matches any single character
- `[a-z]` - matches any lowercase letter between "a" and "z"
- `+` - matches one or more occurrences of the preceding expression
- `?` - matches zero or one occurrence of the preceding expression 
- `()` - used for grouping and capturing specific parts of a pattern

It's important to note that different programming languages may have slightly different syntax for regular expressions, so it's always a good idea to double check the documentation for the specific language you're using.

## See Also

Here are some resources for further reading on regular expressions:

- [Fish Shell's Regex Reference](https://fishshell.com/docs/current/commands.html#set-builtin-regex)
- [Regular Expressions 101](https://regex101.com/)
- [Regex Cheatsheet](https://www.rexegg.com/regex-quickstart.html)

Happy pattern matching!