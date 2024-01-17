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
Calculating a date in the future or past is the process of determining a specific date that is a given number of days, weeks, months, or years before or after a given starting date. Programmers often use this function to schedule tasks or events, track time-sensitive data, or create dynamic reports.

## How to:
To calculate a date in the future or past in Bash, you can use the `date` command with the option `-d` to specify the number of days, weeks, months, or years in the future or past. The code block below shows an example of calculating a date 3 weeks in the future from today's date: 

```Bash 
date -d "+3 weeks"
```
Output: 
```
Sun Mar 1 08:23:25 UTC 2020
```

Similarly, you can also specify a starting date by using the option `-d` followed by the starting date in quotes. The code block below shows an example of calculating a date 2 months and 5 days in the past from a specific starting date:

```Bash
date -d "2020-04-15 -2 months -5 days"
```
Output:
```
Tue Feb 10 08:23:25 UTC 2020
```

## Deep Dive:
1. Historical Context:
Calculating dates in the future or past has been a common programming task for a long time. In the earlier days, developers had to write their own algorithms to perform this calculation. However, with the advancement of programming languages and tools, it has become easier to achieve this function.

2. Alternatives:
Apart from using the `date` command in Bash, there are other alternatives such as using the `add` function in SQL or using libraries such as `datetime` in Python and `moment` in JavaScript.

3. Implementation Details:
The `date` command in Bash accepts a variety of formats for specifying dates, including absolute calendar dates, relative dates, and combinations of both. It also supports various flags and options to format the output according to your preference.

## See Also:
- [Bash Date Command](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [SQL DateTime Functions](https://www.w3schools.com/sql/sql_datatype.asp)
- [Python datetime library](https://docs.python.org/3/library/datetime.html)
- [Moment.js library](https://momentjs.com/)