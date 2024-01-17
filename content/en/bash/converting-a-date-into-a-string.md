---
title:                "Converting a date into a string"
html_title:           "Bash recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? 
Converting a date into a string means taking a date in its numerical or human-readable format and turning it into a string, or sequence of characters. Programmers do this in order to make the date more versatile and easier to work with in their code. By converting a date into a string, programmers can manipulate and format the date in different ways to suit their needs.

## How to: 
To convert a date into a string in Bash, you can use the `date` command with the `+%s` option to get the date in Unix timestamp format. This timestamp represents the number of seconds that have elapsed since January 1, 1970 at 00:00:00 UTC. For example:
```
Bash date "+%s"
1620776000
```

If you want to convert the timestamp back to a human-readable date, you can use the `date` command with the `-d` option and the timestamp value. For example:
```
Bash date -d @1620776412
Wed May 12 10:00:12 UTC 2021
```

## Deep Dive:
Converting a date into a string has been a common practice in programming for a long time. In the early days, dates were often represented as strings in various formats, such as "May 12, 2021" or "05/12/21". However, this led to confusion and errors when trying to compare dates or perform calculations with them. As a result, the Unix timestamp was created as a standardized way of representing a date as a number.

There are alternative ways of converting a date into a string, such as using the `strftime` function in other programming languages. However, Bash's `date` command is a convenient and efficient option for converting dates into their Unix timestamp representation.

## See Also:
- [Bash date command documentation](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Unix timestamp explanation](https://en.wikipedia.org/wiki/Unix_time)