---
title:                "Bash recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why
Comparing two dates may seem like a simple task, but it can actually be quite complicated. When dealing with dates in Bash programming, it is important to have the ability to compare them accurately. This can be useful in a variety of situations, such as sorting files by modification date or checking the age of a file.

## How To
To compare two dates in Bash, we first need to understand how dates are represented. In Bash, dates are usually stored as strings in the format of "YYYY-MM-DD". We can use the `date` command to generate the current date in this format:

```
current_date=$(date +%Y-%m-%d)
```

To compare two dates, we can use the `-lt` (less than) and `-gt` (greater than) operators along with the `date` command. Let's say we have two dates, `date1` and `date2`, and we want to check if `date1` is before `date2`, we can use:

```
if [[ $(date -d "$date1" +%s) -lt $(date -d "$date2" +%s) ]]; then
  echo "$date1 is before $date2"
fi
```

The `date -d` option allows us to convert the date strings into Unix timestamps, which are represented in seconds. This makes it easier to compare the dates using the `-lt` operator. This method also works for checking if a date is after another date, using the `-gt` operator.

We can also compare dates by their month and year, rather than just their full date. For example, if we only want to compare the months of two dates, we can use:

```
if [[ $(date -d "$date1" +%m) -lt $(date -d "$date2" +%m) ]]; then
  echo "$date1 is in an earlier month than $date2"
fi
```

## Deep Dive
When comparing dates, it is important to be aware of any potential issues with the date format. For example, using the `-lt` and `-gt` operators may give unexpected results if the dates are not in the same format. Additionally, leap years can also cause issues when comparing dates. These are important considerations to keep in mind when writing scripts that involve date comparisons.

Another useful tool for comparing dates is the `diff` command, which can show the difference between two dates in various units such as days, weeks, or months. For more complex date calculations, the `datecalc` utility can also be helpful.

## See Also
- https://www.gnu.org/software/coreutils/manual/html_node/Single-character-options.html#Single-character-options
- https://www.gnu.org/software/coreutils/manual/html_node/Timestamp-conversion.html#Timestamp-conversion
- https://www.linuxjournal.com/content/working-date-and-time-bash-2
- https://www.cyberciti.biz/faq/unix-linux-shell-convert-string-to-int/
- https://www.gnu.org/software/coreutils/manual/html_node/Comparison-of-files-timestamp.html#Comparison-of-files-timestamp