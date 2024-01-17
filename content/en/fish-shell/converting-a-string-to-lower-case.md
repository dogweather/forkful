---
title:                "Converting a string to lower case"
html_title:           "Fish Shell recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?

Converting a string to lower case simply means changing all the letters in a string to their lower case equivalent. Programmers often do this to ensure consistency in their code and make it easier to compare and manipulate strings.

## How to:

Fish Shell makes it easy to convert a string to lower case using the "string tolower" command. Here's an example:

```
set my_string "Hello World"
string tolower $my_string
```

This will output "hello world" in the terminal.

If you want to convert a specific part of the string, you can use the "|string tolower" command. For example:

```
set my_string "Hello World"
echo $my_string | string tolower
```

This will output "hello world" in the terminal as well.

## Deep Dive:

Converting strings to lower case dates back to early computer systems, where data was stored in all uppercase letters for technical reasons. As computers evolved, lowercase letters became more common in programming languages but the convention of storing data in uppercase remained. This led to the need for converting strings to lowercase in order to easily compare and manipulate data.

While Fish Shell makes it easy to convert strings to lower case, there are other methods such as using built-in functions in other programming languages like Python and JavaScript. However, these methods may require more code and may not be as efficient as using the dedicated "string tolower" command in Fish Shell.

## See Also:

- [Fish Shell documentation](https://fishshell.com/docs/current/cmds/string.html)
- [Python lower() function](https://www.w3schools.com/python/ref_string_lower.asp)
- [JavaScript toLowerCase() method](https://www.w3schools.com/jsref/jsref_tolowercase.asp)