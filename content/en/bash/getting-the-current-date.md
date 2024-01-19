---
title:                "Getting the current date"
html_title:           "Elm recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Getting the Current Date in Bash Programming

## What & Why?
Extracting the current date in Bash signifies retrieving the real-time date as per the system clock. This capability comes handy for various elements of scripting such as timestamps in logs, scheduling tasks, or data comparison.

## How to:
Getting the current date is a breeze. The command is:
```Bash
date
```
Here's what the output might look like:
```Bash
Tue Sep 28 20:02:55 PDT 2021
```
For a more refined output, like just the date:
```Bash
date "+%Y-%m-%d"
```
You'd get the output like:
```Bash
2021-09-28
```
## Deep Dive
Historically, in the Unix-like systems, the 'date' command was included as a must-have utility in the operating systems from the early times. Even with the inception of Linux, and subsequently Bash, this command retained its relevance.

While the 'date' command is standard, you can also rely on the 'printf' function, or even delve into Python or Perl within your Bash scripts for formatting dates.

`date` command implementation details mainly surround formatting. With '+%Y-%m-%d', we are telling `date` to display the year (`%Y`), month (`%m`), and day (`%d`). You may experiment with different format specifiers to meet your requirement.

## See Also
For additional reading and examples, refer to the following sources:

- [GNU Coreutils: Date Documentation](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [BASH Programming - Introduction](https://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO-2.html)
- [Unix `date` Command](https://ss64.com/bash/date.html)