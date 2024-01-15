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

## Why

Capitalizing strings in Bash can be useful for various reasons, such as standardizing user input or displaying data in a consistent format. It not only makes your code more readable but also improves its overall functionality.

## How To

To capitalize a string in Bash, we can use the `tr` command along with the `[:lower:]` and `[:upper:]` options. First, we need to store the string in a variable. Let's use the name "john" as an example:

```Bash
name="john"
```

Next, we will use the `tr` command to capitalize the first letter of the string:

```Bash
echo "$name" | tr '[:lower:]' '[:upper:]'
```

This will output "JOHN". To capitalize the entire string, we can use the `tr` command again, but this time we will use the `[:upper:]` option twice:

```Bash
echo "$name" | tr '[:lower:]' '[:upper:]' | tr '[:upper:]' '[:lower:]'
```

The above command will output "JOHN". Alternatively, we can also use the `toupper` and `tolower` functions in Bash:

```Bash
toupper "$name"
tolower "$name"
```

Both of these commands will output "JOHN". Keep in mind that these methods of capitalizing a string will only work for ASCII characters.

## Deep Dive

Bash provides various built-in string manipulation functions that can be used to capitalize strings. These include `strtoupper` and `strtolower` which work similar to their counterparts `toupper` and `tolower` but also support non-ASCII characters.

Additionally, we can use parameter expansion to capitalize the first letter of a string like so:

```Bash
name="john"
echo "${name^}"
```

This will output "John". To capitalize the entire string, we can simply use two caret symbols:

```Bash
name="john"
echo "${name^^}"
```

This will output "JOHN". Parameter expansion provides more flexibility when it comes to manipulating strings in Bash.

## See Also

- Full list of `tr` commands: https://www.gnu.org/software/coreutils/tr
- Bash information and documentation: https://www.gnu.org/software/bash/