---
title:                "Finding the length of a string"
html_title:           "Arduino recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Finding the length of a string checks how many characters a string has. Often, programmers do this for validation, truncation, or alignment.

## How to:

In Bash, the length of a string can be obtained using the `${#string}` syntax. Let's say we have a string `hello`.

```Bash
string="hello"
echo ${#string}
```

The output will be `5` indicating that the string `hello` comprises of 5 characters.

## Deep Dive

The `${#string}` syntax is historical and stems from Bash's Unix origins. Personal Computing History notes that this syntax existed in the Unix shell well before Bash was developed in 1989.

There are alternative ways to find string length:

1. `expr length $string` : The `expr` command also calculates the length.
2. `echo -n $string | wc -c` : The `wc` command (word, line, character, and byte count) with `-c` option counts the number of characters.

In Bash, finding the string length is efficient due to its internal implementation. When a string variable is set, Bash keeps track of its length and simply needs to retrieve it. Internally, the `${#string}` syntax triggers Bash to give you the length tracked when the variable was set.

## See Also

For more detailed info about string operations in Bash:
- Greg's Wiki: http://mywiki.wooledge.org/BashGuide/Parameters
- The Linux Documentation Project: https://www.tldp.org/LDP/abs/html/string-manipulation.html
- Personal Computing History: http://www.computinghistory.org.uk/det/6128/Bash-Bourne-Again-Shell/