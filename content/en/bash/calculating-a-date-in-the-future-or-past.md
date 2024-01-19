---
title:                "Calculating a date in the future or past"
html_title:           "Bash recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?

Calculating a date in the future or past means adjusting a given date by a certain time period. Programmers perform this operation to schedule actions, handle temporal data, or build features like reminders and counters.

## How to:

Bash has `date` command to do this magic. Here, we will use `date -d` option that reads input and prints corresponding date/time.

* Current date

```Bash
date '+%Y-%m-%d'
```
Output:
```Bash
2022-02-28
```

* Add 1 day

```Bash
date -d "+1 day" '+%Y-%m-%d'
```
Output:
```Bash
2022-03-01
```

* Subtract 2 weeks

```Bash
date -d "-2 week" '+%Y-%m-%d'
```
Output:
```Bash
2022-02-14
```

## Deep Dive

Historical Context: Calculating dates has been a universal need across software and cultures since the early days of computing. `date` command is a part of GNU Core Utilities since 1994, enabling date-time operations.

Alternatives: There are other ways to calculate dates in Shell scripts, like using Perl, Python, or PHP CLI. But sticking to built-in tools like `date` keeps your scripts light and portable.

Implementation Details: `date -d` option works by feeding the string into GNU `date`'s date parser, which identifies and applies time units like 'weeks', 'days', etc. This handy feature allows for flexible date manipulations but isn't required by POSIX standards, so always test when porting.

## See Also

* man date: https://www.man7.org/linux/man-pages/man1/date.1.html
* Date Arithmetic: https://www.gnu.org/software/coreutils/date
* Alternatives to date command: https://www.cyberciti.biz/faq/howto-get-current-date-time-in-shell-script/