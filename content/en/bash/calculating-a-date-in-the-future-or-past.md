---
title:                "Bash recipe: Calculating a date in the future or past"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why
Calculating dates in the future or past is an essential skill for any Bash programmer. Whether you are creating a script to schedule tasks or trying to determine the expiration date of a file, knowing how to manipulate dates can greatly improve your efficiency and productivity.

## How To
Calculating dates in the future or past may seem daunting at first, but with the right tools and techniques, it can be a simple task. First, we need to understand how dates are represented in Bash. Dates are represented as seconds since January 1, 1970, also known as Unix time. This means that any date can be represented as a specific number of seconds since that date.

To calculate a date in the future, we can use the `date` command, coupled with the `+%s` flag, which displays the date in seconds. For example, to calculate the date 5 days from today, we can use the following code:

```Bash
future_date=$(date -d "+5 days" +%s)
```

This will store the future date in seconds in the variable `future_date`. We can then convert this back to a human-readable format using the `date` command with the `-d` flag:

```Bash
date -d @$future_date
```

Similarly, to calculate a date in the past, we can use the `-` operator instead of `+`:

```Bash
past_date=$(date -d "-1 week" +%s)
```

This will calculate the date 1 week ago and store it in the `past_date` variable. Again, we can convert it back to a readable format using the `date` command.

## Deep Dive
There are also other ways to perform date calculations in Bash, such as using the `bc` command, which stands for "basic calculator." This allows us to perform complex calculations involving dates, such as determining the difference between two dates or adding/subtracting a specific amount of time to a given date.

We can also use the `date` command with the `-d` flag to specify a specific date and time, instead of using relative values like "5 days" or "1 week." This gives us more flexibility in calculating dates and allows us to perform date calculations even for dates far in the past or future.

## See Also
- [Bash documentation on the `date` command](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html#date-invocation)
- [A tutorial on using the `bc` command for date calculations](https://www.computerhope.com/unix/ubc.htm)
- [A comprehensive guide to date calculations in Bash](https://www.linuxjournal.com/article/1425)