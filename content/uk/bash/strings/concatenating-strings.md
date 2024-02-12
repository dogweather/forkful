---
title:                "Об'єднання рядків"
aliases:
- /uk/bash/concatenating-strings.md
date:                  2024-01-20T17:34:14.697080-07:00
model:                 gpt-4-1106-preview
simple_title:         "Об'єднання рядків"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/concatenating-strings.md"
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
