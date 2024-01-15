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

## Why
Calculating a date in the future or past can be useful for a variety of reasons, such as scheduling events, budget planning, or tracking deadlines. With Bash programming, you can easily automate this task and save time on manual calculations.

## How To
Using the `date` command in Bash, you can easily calculate a date in the future or past. Here's an example of adding 10 days to the current date:
```Bash
date -d "+10 days"
```
This will output the new date in the format of "day of the week, month day, year". To specify a different starting date, you can use the `-d` option and specify the date in the format of "month/day/year". For instance, if you want to calculate the date 1 year and 3 months from October 21, 2021, the command would be:
```Bash
date -d "10/21/2021 +1 year +3 months"
```
The output will be "Thursday, January 21, 2023". You can also specify a specific time, such as adding 5 hours and 30 minutes to the current time:
```Bash
date -d "+5 hours 30 minutes"
```
The output will be in the format of "hour:minute:second AM/PM". Other useful options for calculating dates include `-d "yesterday"` or `-d "tomorrow"`.

## Deep Dive
The `date` command in Bash uses the system's default date and time settings. However, you can also specify a different time zone by using the `-u` option and specifying the time zone as a numerical offset. For example, to calculate a date in the future or past in Pacific Standard Time, which is 8 hours behind the current time zone, the command would be:
```Bash
date -d "-1 day" -u -8 hours
```
This would output the date and time in Pacific Standard Time. Additionally, you can use wildcards such as `*` to calculate dates relative to the current date. For example, to calculate the last day of the current month, the command would be:
```Bash
date -d "`date -d 'next month' +*days -1 day`"
```
This uses the `date` command within the command to calculate the end date of the next month, and then subtracts one day to get the last day of the current month.

## See Also
- [Bash Reference Manual: `date` command](https://www.gnu.org/software/bash/manual/html_node/datetime.html#Shell-Conditional-Expressions)
- [BashGuide: Dates and Times](https://mywiki.wooledge.org/BashGuide/Datetime)
- [Linuxize: How to Use the `date` Command in Linux](https://linuxize.com/post/how-to-use-linux-date-command/)