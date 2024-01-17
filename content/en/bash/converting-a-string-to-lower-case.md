---
title:                "Converting a string to lower case"
html_title:           "Bash recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?

Converting a string to lower case is a common task in programming, where we change all the uppercase letters in a string to lowercase. This can be useful for comparing strings without worrying about case sensitivity or for ensuring consistency in data. Sometimes, we also want our output to be in lower case for aesthetic or formatting purposes.

## How to:

To convert a string to lower case in Bash, we can use the built-in command `tr`. This command translates characters in a string into the specified characters. In this case, we will use `tr '[:upper:]' '[:lower:]'` to translate all uppercase letters to lowercase.

```Bash
# Sample string
str="Hello World!"

# Convert all letters to lowercase
lower=$(echo $str | tr '[:upper:]' '[:lower:]')

# Print output
echo $lower # hello world!
```

Alternatively, we can use the `tr [:upper:] [:lower:]` command directly on a string variable without using `echo`:

```Bash
# Sample string
str="Hello World!"

# Convert all letters to lowercase
echo $str | tr [:upper:] [:lower:] # hello world!
```

## Deep Dive:

Converting a string to lower case has been a functionality supported by most programming languages for a long time, as it is a simple yet essential task. This feature first appeared in the programming language Fortran in the late 1950s, and it has since been implemented in various forms in other languages.

Apart from using the `tr` command, we can also convert a string to lower case using built-in Bash parameter expansion `${var,,}`, which converts the contents of variable `var` to lowercase. However, this method only works in newer versions of Bash (version 4 and above).

## See Also:

- Bash documentation for the `tr` command: https://www.gnu.org/software/bash/manual/html_node/Shell-Builtin-Commands.html
- Bash documentation for parameter expansion: https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html#Shell-Parameter-Expansion