---
title:                "Parsing a date from a string"
html_title:           "Bash recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing a date from a string is the process of extracting specific date information, such as month, day, and year, from a string of text. Programmers often do this to convert human-readable dates into a format that the computer can understand and manipulate.

## How to:

```Bash
# Basic Command
date -d "1 week ago" 

# Output:
Sun Jul 25 09:49:24 PDT 2021

# Recreating date 
date -d "July 10, 1990"

# Output:
Tue Jul 10 00:00:00 PDT 1990

# Using awk command
awk 'BEGIN{print strftime("%a %b %d %T %Z %Y", mktime("2020 01 01 00 00 00"))}'

# Output:
Tue Jan 01 00:00:00 PST 2020 
```

## Deep Dive

Parsing dates has been a common task in programming since the early days of computing. In the past, programmers had to manually extract and manipulate date information from strings, which was a time-consuming and error-prone process. With the development of modern programming languages and tools, the process has become much easier and more efficient.

An alternative to using the `date` command in Bash could be using a scripting language like Python or Perl that has built-in date parsing functions. This option may be more suitable for complex date parsing tasks, but for simple tasks, using the `date` command in Bash is a quick and easy solution.

To understand how Bash parses dates, we need to look at Unix time, also known as Epoch time. Unix time is a system for describing a point in time as the number of seconds that have elapsed since January 1, 1970, at 00:00:00 UTC. Bash uses this system to convert human-readable dates to a standardized format that can be easily manipulated by the computer.

## See Also

- [Bash Date Command Documentation](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Unix Time Wikipedia Page](https://en.wikipedia.org/wiki/Unix_time)
- [Python's datetime Library Documentation](https://docs.python.org/3/library/datetime.html)