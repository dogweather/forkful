---
title:                "Parsing a date from a string"
date:                  2024-01-20T15:34:32.513317-07:00
html_title:           "Arduino recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"

category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing a date from a string means extracting the date components—like day, month, and year—from text. Programmers do it to manipulate or display dates in different formats or to calculate time differences in scripts.

## How to:

Using `date` with `+%Y-%m-%d` gives us a formatted output:

```Bash
date_str="Jan 01 2023"
formatted_date=$(date -d "$date_str" '+%Y-%m-%d')
echo $formatted_date
```
```
2023-01-01
```

`date -d` lets us parse our string, while `+%Y-%m-%d` specifies the output format.

## Deep Dive

Bash itself isn't great at date parsing. Historically, Unix systems didn't include a built-in for this. Most scripts relied on external tools or complex workarounds. GNU `date` changed the game with its `-d` option, allowing easy date parsing and output formatting.

Alternatives? Sure, there's `awk`, `sed`, and `perl`. Each has its own way of tackling the problem, but `date` is typically the first choice due to simplicity.

Implementation details get spicier. `date` uses system locale settings by default, affecting how it interprets input. Overriding locale may be necessary for consistent behavior across different environments. Plus, handling dates before 1970 or after 2038? That's where things can get buggy due to Unix timestamp constraints.

## See Also

- GNU `date` man page: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- More on Unix timestamp and the Y2038 problem: https://en.wikipedia.org/wiki/Year_2038_problem
- Date parsing in `awk`: https://www.gnu.org/software/gawk/manual/html_node/Time-Functions.html
