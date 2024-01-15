---
title:                "Calculating a date in the future or past"
html_title:           "Fish Shell recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why
Ever had to figure out a date in the future or the past? Maybe you were planning a trip or trying to schedule an event. Either way, manually calculating dates can be time-consuming and prone to errors. With Fish Shell, you can easily automate this process and save yourself some headache.

## How To
Calculating a date in the future or past using Fish Shell is a breeze. Simply follow the steps below:

1. Open your terminal and enter the Fish Shell by typing `fish`.
2. Use the `date` command followed by the desired date in YYYY-MM-DD format. For example, `date 2021-12-25` will output the date for Christmas in the current year.
3. To calculate a date in the past, use a negative number after the date. For example, `date 2021-12-25 -1` will output the date from the previous year.
4. You can also use keywords like `today` or `tomorrow` instead of a specific date. For example, `date tomorrow` will output tomorrow's date.

Below is a sample output of the above commands:

```
Fish Shell 3.2.2
Copyright (c) 2015 - 2020 Fish contributors

Date Output:
2021-12-25
2020-12-25
2021-06-08
```

## Deep Dive
Behind the scenes, Fish Shell uses the GNU `date` command to calculate dates. It also accepts date formats from other languages, making it a versatile tool for international users. Additionally, you can use flags such as `-d` or `--date` to specify a different starting date or time zone.

See Also
- [Fish Shell official website](https://fishshell.com)
- [GNU date command documentation](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Fish Shell cheat sheet](https://fishshell.com/docs/current/tutorial.html#quick-reference)