---
title:                "Comparing two dates"
html_title:           "Arduino recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

# BASH Guide: Comparing Dates - Why, How & Beyond

## What & Why?

Date comparison in Bash involves assessing two or more dates in relation to each other to determine whether one is earlier, later or the same. This is vital for programmers for automating tasks concerning scheduling, file backups, data archiving, and much more.

## How to:

Use the Unix `date` command to extract current date, format it and compare it with a specific date.
  
```bash
date1=$(date -d "2021-10-19" +%s) 
date2=$(date -d "2021-12-19" +%s) 

if [ $date1 -lt $date2 ]; then
  echo "date1 is earlier than date2"
else
  echo "dates are equal or date1 is later"
fi
```
This script converts each date to Unix Epoch time (seconds passed since 1970-01-01 00:00:00 UTC, ignores leap seconds). Bash can then compare these integers.

## Deep Dive

Historically, Bash didn't inherently support date comparison, so many programmers used external commands or languages, like Perl or Python. However, Bash has evolved and using `date` command along with `if/else` construct is a straightforward and efficient approach.

Some alternatives still include utilizing `strtotime` function in PHP or using Date objects in Javascript. But Bash obviates the need of any such second language.

Inside Bash, it's all about time stamp conversion. The `date -d` command takes a string and converts it into a date. The `+%s` formatter converts this date into Unix Epoch format that can finally be compared in an integer format.

## See Also 

1. [The "date" man page](https://man7.org/linux/man-pages/man1/date.1.html): Detailed descriptions of display formats and command-line options.
2. [GNU Coreutils](https://www.gnu.org/software/coreutils/coreutils.html) :  Basic file, shell, and text manipulation commands of the GNU operating system.
3. [Unix StackExchange](https://unix.stackexchange.com/questions/321267/how-should-i-compare-dates-in-bash): Community forums for more real-life solutions and answers related to Unix & Linux.