---
date: 2024-01-20 17:32:47.266608-07:00
description: "Comparing two dates means checking if one date is earlier, the same,\
  \ or later than another. Programmers do this to organize events, validate input,\
  \ and\u2026"
lastmod: '2024-03-13T22:45:00.485590-06:00'
model: gpt-4-1106-preview
summary: Comparing two dates means checking if one date is earlier, the same, or later
  than another.
title: Comparing two dates
weight: 27
---

## How to:
With Fish Shell, we can compare two dates using the `date` command. Below are examples.

```fish
# Getting the current date in seconds since epoch
set current_date (date +%s)

# Converting a specific date to seconds since epoch
set specific_date (date -d "2023-04-01" +%s)

# Comparing the dates
if test $specific_date -lt $current_date
    echo "Specific date is earlier than the current date."
else if test $specific_date -eq $current_date
    echo "Dates are the same."
else
    echo "Specific date is later than the current date."
end
```
Sample output if the current date is past April 1st, 2023:
```
Specific date is earlier than the current date.
```

## Deep Dive
Historically, comparing dates in programming has been a bit of a hassle due to various date formats and time zones. Fish Shell simplifies this task with its built-in `date` function, converting dates to seconds since the Unix epoch (January 1, 1970). This gives us a universal point in time to compare against.

Alternatives to Fish Shell for comparing dates include scripting languages like Python or using `date` manipulation tools available in Unix-based systems, like `dateutil` in GNU core utilities (coreutils). Implementation-wise, when we use `date +%s`, Fish internally calls the system `date` command, which is why it's so effective cross-platform.

Comparing dates is also essential for cron jobs, backup scripts, and time-based access control. Being comfy with date comparisons means smoother automation and fewer temporal bugs.

## See Also
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [GNU Coreutils: Date](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [The Unix Epoch Time](https://en.wikipedia.org/wiki/Unix_time)
