---
title:                "Capitalizing a string"
html_title:           "Bash recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Capitalizing a string in Bash means converting all letters in a string to uppercase. Programmers may do this to normalize input data or for stylistic purposes in their code.

## How to:

To capitalize a string in Bash, we can use the built-in ```tr``` command along with the ```[:lower:]``` and ```[:upper:]``` character classes.

```Bash
# Syntax:
# echo [string] | tr [from_chars] [to_chars]

echo "hello world" | tr [:lower:] [:upper:]

# Output:
HELLO WORLD
```

Another way to capitalize a string is by using the parameter expansion feature in Bash. We can use the ```^``` character to capitalize the first letter and ```^^``` to capitalize all letters.

```Bash
# Syntax:
# ${parameter^} or ${parameter^^}

lowercase="hello world"
echo "${lowercase^}"
echo "${lowercase^^}"

# Output:
Hello world
HELLO WORLD
```

## Deep Dive:

The ```tr``` command has been available in Unix and Unix-like systems since the 1970s and is commonly used for text manipulation. It stands for "translate" and can be used to convert one set of characters to another.

Alternative ways to capitalize a string in Bash include using command substitution or using external programs like ```awk``` or ```sed```. However, the ```tr``` method is the most concise and efficient.

The Bash parameter expansion feature was added in version 4.0 and offers more flexibility in manipulating strings. Apart from capitalizing, we can also convert to lowercase or perform other modifications.

## See Also:

- [Bash Documentation](https://www.gnu.org/software/bash/manual/bash.html)
- [tr command in Unix Wiki](https://en.wikipedia.org/wiki/Tr_(Unix))
- [Bash Parameter Expansion](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html#Shell-Parameter-Expansion)