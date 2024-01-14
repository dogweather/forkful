---
title:                "Fish Shell recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to know the current date while coding in Fish Shell? Maybe you want to include today's date in a file name or use it in a conditional statement. Whatever the reason, knowing how to get the current date can come in handy when programming in Fish Shell.

## How To

To get the current date in Fish Shell, you can use the `date` command with the `+%Y-%m-%d` option. This will output the date in a year-month-day format. For example, if today's date is April 5th, 2021, the output would be `2021-04-05`.

```Fish Shell
date +%Y-%m-%d
```

You can also customize the format of the date by using different options with the `date` command. For example, if you want the date in a month-day-year format, you can use the `+%m-%d-%Y` option. The output would then be `04-05-2021`.

```Fish Shell
date +%m-%d-%Y
```

Another option is to use the `date` command with the `-I` option, which will output the date in the ISO 8601 format (YYYY-MM-DD). This is useful if you want to sort files by date.

```Fish Shell
date -I
```

You can also add a time to the date output by using the `+%H:%M:%S` option. This will give you the current time along with the date.

```Fish Shell
date +%Y-%m-%d" "%H:%M:%S
```

## Deep Dive

The `date` command is part of the GNU Core Utilities, which is a set of common tools used in Unix-like operating systems. It is used to display or set the system date and time.

The `+%Y`, `+%m`, `+%d`, `+%H`, `+%M`, and `+%S` options represent the year, month, day, hour, minute, and second, respectively. You can use other options to output different pieces of date information, such as the day of the week, the week number, or the time zone.

## See Also

- [Fish Shell official website](https://fishshell.com/)
- [GNU Core Utilities manual](https://www.gnu.org/software/coreutils/manual/coreutils.html#Date-invocation)
- [Unix Timestamp](https://en.wikipedia.org/wiki/Unix_time)