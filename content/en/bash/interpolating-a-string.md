---
title:                "Interpolating a string"
html_title:           "Bash recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Interpolating a string means inserting content, such as variables or commands, into a pre-defined string. Programmers use this technique to create dynamic output and make their code more efficient.

## How to:

To interpolate a string in Bash, we can use the `echo` command with the `-e` option. Here's an example:

```
name="John"
echo -e "Hello, $name. How are you today?"
```

This will output: `Hello, John. How are you today?`.

We can also use the `printf` command to interpolate strings. Here's an example:

```
age=25
printf "I am %s years old." "$age"
```

This will output: `I am 25 years old.`.

## Deep Dive:

Interpolating strings is a common technique used in programming languages to format dynamic output. It first originated in the Perl programming language and has since been adopted by many other languages, including Bash.

An alternative to string interpolation in Bash is using the concatenation operator `.` to join strings together. However, this can become cumbersome when dealing with multiple variables or long strings.

When interpolating strings in Bash, we must be careful with special characters that may interfere with the content being inserted, such as the dollar sign `$`. To avoid this, we can use single quotes instead of double quotes in our strings.

## See Also:

To learn more about string interpolation in Bash, check out the official Bash manual: https://www.gnu.org/software/bash/manual/html_node/Quoting.html#Quoting.

You can also explore other string manipulation techniques in Bash, like substitution and pattern matching: https://www.tldp.org/LDP/abs/html/string-manipulation.html.