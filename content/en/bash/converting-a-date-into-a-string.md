---
title:                "Converting a date into a string"
html_title:           "Arduino recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Converting a date into a string in Bash refers to the process of taking a specific date format and transforming it into a textual representation. Programmers do it to make dates easier to manipulate, display, or store in a text-friendly format.

## How to:

In Bash, we use the built-in `date` command to manipulate date and time. Let's convert the current date into a string:

```Bash
date_now=$(date)
echo $date_now
```

The `date` command without any arguments prints the current date and time. For custom date strings, `-d` option helps to specify any date and `'+%Y-%m-%d'` formats the output:

```Bash
custom_date=$(date -d '2022-01-01' '+%Y-%m-%d')
echo $custom_date
```

## Deep Dive

The `date` command has been a part of Unix-like operating systems since the early days, arriving with the first version of Bash in 1989. It is one of the most flexible commands, proven by its use in countless scripts and shell sessions over decades.

Inside Bash, there are no real alternatives to `date` for date string conversions, however in languages like Python or JavaScript, date objects and methods can be used.

The `date` command under the hood uses the system's builtin libraries to convert and manipulate dates, which makes it quite efficient. The command also considers system time zone settings and locale for accurate conversions.

## See Also

To dive deeper into the `date` command, consider the following resources:

- `man date` : Use the command line to access the manual page for `date` itself.
- [GNU Coreutils](https://www.gnu.org/software/coreutils/date) : The official GNU page on `date`.
- [Bash Date Command](https://linuxize.com/post/bash-date-command/) : A comprehensive guide to the `date` command.
- [Advanced Bash-Scripting Guide](https://tldp.org/LDP/abs/html/) : A guide for advanced bash scripting.