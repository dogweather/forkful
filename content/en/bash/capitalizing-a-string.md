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

Capitalizing a string in programming refers to converting the first letter of each word in a string to uppercase. Programmers usually do this to format text for readability, especially for titles, headings, or when presenting data to end-users.

## How to:

The Bash commands to capitalize a string depend on string manipulation capabilities of the shell. In the latest version of Bash, you use string substitution. Here's how:

```Bash
string="hello, world"
echo "${string^}"
```

This would output:

```Bash
Hello, world
```

To capitalize every word in a string:

```Bash
echo "${string^^}"
```

Which would output:

```Bash
HELLO, WORLD
```

## Deep Dive

Historically, string manipulation wasn't Bash's strong suit. Early versions didn't support it. However, with Bash 4.0, things changed. Features like string substitution were introduced, making it easier to manipulate strings.

However, Bash isn't the only way. Other programming languages like Python, Java, JavaScript etc., have built-in methods to capitalize strings. They can often be more efficient, especially for complicated manipulations.

As for implementation, when Bash capitalizes a string, it changes the ASCII value of the lowercase characters to their uppercase equivalents. Specifically, it subtracts 32 from the ASCII value of each lowercase letter, which gives the uppercase equivalent - this happens behind the scenes when you use the "^" or "^^" substitution operators.

## See Also

Here are a few resources for more in-depth information:


For capitalizing strings in other programming languages:

2. [JavaScript: Capitalizing Strings](https://www.w3schools.com/jsref/jsref_touppercase.asp)
3. [Java: String Manipulation](https://docs.oracle.com/javase/tutorial/java/data/strings.html)