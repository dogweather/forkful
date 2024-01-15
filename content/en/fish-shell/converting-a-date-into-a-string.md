---
title:                "Converting a date into a string"
html_title:           "Fish Shell recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why
Dates are a vital part of our day-to-day lives, and being able to manipulate them in a program is essential. By converting a date into a string, we can easily display it in a format that is both readable and visually pleasing.

## How To
To convert a date into a string using the Fish Shell, we can use the `date` command with the `+%s` flag to output it in a UNIX timestamp format.

```
Fish Shell  date +%s
1633966146
```

This timestamp can then be formatted using the `date` command again, with the desired output format specified by the `+FORMAT` flag.

```
Fish Shell  date -r 1633966146 +"%B %d, %Y"
October 11, 2021
```

## Deep Dive
Converting a date into a string involves breaking it down into its individual components and then formatting it accordingly. The `date` command in the Fish Shell allows for various options to manipulate dates, such as adding or subtracting days, months, or years, and specifying time zones.

To convert a date into a string in a specific format, we can use the `+FORMAT` flag with the `date` command. Some common formatting options include `%Y` for the year, `%m` for the month, `%d` for the day, `%H` for the hour, `%M` for the minute, and `%S` for the second.

```
Fish Shell  date -r 1633966146 +"%I:%M %p"
12:29 PM
```

Additionally, Fish Shell also allows for relative time formatting, such as displaying the date as “x days ago” or “x hours ago.” This can be achieved by adding the `--relative` flag to the `date` command.

## See Also
- [Fish Shell documentation on the date command](https://fishshell.com/docs/current/cmds/date.html)
- [Unix timestamp](https://www.unixtimestamp.com/)
- [List of time formatting options](https://strftime.org/)