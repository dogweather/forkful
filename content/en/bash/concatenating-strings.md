---
title:                "Concatenating strings"
html_title:           "Bash recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

# What & Why?

Concatenating strings in Bash is the process of combining multiple strings into a single string. Programmers often use this in order to create dynamic output or build complex commands.

#How to:

Concatenating strings in Bash is done using the `echo` command. For example, to combine the strings "Hello" and "World", we would use the following code:
```
Bash
echo "Hello""World"
```

This would output: `HelloWorld`. We can also include variables in our string concatenation. For instance:
```
Bash
name="John"
echo "Hello "$name"!"
```

This would output: `Hello John!` 

# Deep Dive:

String concatenation has been around since the early days of programming and can be found in many different programming languages, including Bash. One alternative to using the `echo` command for string concatenation is using the `printf` command, which allows for more control over the output formatting.

When concatenating strings, it is important to note that there should be no spaces between the strings or variables being combined. If a space is needed, it should be included within one of the strings using quotes. Additionally, Bash does not have a built-in function for string concatenation, so using commands such as `cat` or `sed` can also achieve similar results.

# See Also:

For more information on string concatenation in Bash, check out the [Bash String Manipulation](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Shell-Parameter-Expansion) section of the Bash manual. Another useful resource is [tldp.org](https://www.tldp.org/LDP/abs/html/string-manipulation.html) which provides more in-depth examples and explanations.