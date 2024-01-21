---
title:                "Інтерполяція рядків"
date:                  2024-01-20T17:50:31.565269-07:00
model:                 gpt-4-1106-preview
simple_title:         "Інтерполяція рядків"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Що і чому?)
Interpolating a string means inserting variables or expressions right into a string. Programmers do it to dynamically build or customize messages, configurations, or code.

## How to: (Як це зробити:)
In Bash, place the variable name inside `${}` or directly after a `$` within double quotes.

```Bash
# Basic variable interpolation
name="Olena"
echo "Hello, $name!"

# Using braces for clarity or when needed
greeting="Hello, ${name}!"
echo $greeting
```
Output:
```
Hello, Olena!
Hello, Olena!
```

To combine variables or include a variable next to other characters without spaces:

```Bash
# Concatenate variables
user="Olena"
action="logged in"
echo "$user$action"

# Variable with a suffix
file_count=3
echo "You have ${file_count} files."
```
Output:
```
Olena logged in
You have 3 files.
```

## Deep Dive (Поглиблений огляд)
String interpolation is not unique to Bash; it's a feature of many programming languages, used since the early days to ease code readability and functionality. In Bash, unlike languages like PHP or Perl, you don't need a special syntax (like `.` or `,`) to concatenate strings and variables; instead, it's mainly a feature of how the shell expands variables within strings. As an alternative to direct interpolation, you might use command substitution (using the `$(...)` syntax) to place command output directly into a string.

```Bash
# Command substitution example
echo "The date is $(date)."
```

In terms of performance, interpolation may be slightly faster than other techniques, like multiple `echo` statements or the concatenation of variables using command substitution, because it's handled directly by the shell's parser. But remember, with great power comes great responsibility – interpolating user input directly can lead to security issues like injection attacks unless properly sanitized.

## See Also (Дивіться також)
For more details and best practices, here are some useful resources:

- Bash manual about parameter expansion: [GNU Bash manual](https://www.gnu.org/software/bash/manual/)
- Advanced Bash-Scripting Guide about string manipulation: [tldp Advanced Bash-Scripting Guide](https://tldp.org/LDP/abs/html/string-manipulation.html)
- The dangers of unsanitized input in code interpolation: [OWASP Untrusted Data](https://owasp.org/www-community/attacks/Command_Injection)