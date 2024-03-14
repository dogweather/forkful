---
date: 2024-01-20 17:34:14.697080-07:00
description: Concatenating strings means sticking them together end-to-end. Programmers
  do it to build up texts, commands, or data based on logic and input.
lastmod: '2024-03-13T22:44:49.557975-06:00'
model: gpt-4-1106-preview
summary: Concatenating strings means sticking them together end-to-end. Programmers
  do it to build up texts, commands, or data based on logic and input.
title: "\u041E\u0431'\u0454\u0434\u043D\u0430\u043D\u043D\u044F \u0440\u044F\u0434\
  \u043A\u0456\u0432"
---

{{< edit_this_page >}}

## What & Why? (Що та Навіщо?)
Concatenating strings means sticking them together end-to-end. Programmers do it to build up texts, commands, or data based on logic and input.

## How to: (Як це робити:)
Bash lets you concatenate strings easily. Here are some straightforward examples:

```Bash
# Direct concatenation
greeting="Hello, "
name="Oleksiy"
welcome_message=$greeting$name
echo $welcome_message
# Output: Hello, Oleksiy

# Using curly braces for clarity
full_greeting="${greeting}there, ${name}!"
echo $full_greeting
# Output: Hello, there, Oleksiy!

# Concatenating with a variable update
suffix=" How are you?"
welcome_message+=$suffix
echo $welcome_message
# Output: Hello, Oleksiy How are you?
```

## Deep Dive (Занурення глибше):
In early computer programming, memory was precious. Concatenating strings had to be done carefully. Today’s Bash doesn't sweat the small stuff - it's easy and efficient.

Alternatives to Bash's direct string concatenation include using `printf`, `echo`, or command substitution:

```Bash
# Using printf
printf -v full_message "%s%s" "$greeting" "$name"
echo $full_message
# Output: Hello, Oleksiy

# Concatenation with command substitution
date_prefix=$(date +"%Y-%m-%d")
log_message="Log entry for $date_prefix: All is well."
echo $log_message
# Output: Log entry for 2023-03-15: All is well.
```

Behind the scenes, Bash handles string concatenation by simply laying one string after another in memory, letting you treat multiple strings as one.

## See Also (Дивіться також):
- [Bash String Manipulation Guide](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
- [Advanced Bash-Scripting Guide](http://www.tldp.org/LDP/abs/html/string-manipulation.html)
- [Bash Reference Manual](https://www.gnu.org/software/bash/manual/)
