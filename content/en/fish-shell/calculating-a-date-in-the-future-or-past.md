---
title:                "Fish Shell recipe: Calculating a date in the future or past"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why
As a programmer, it is often necessary to calculate dates in the future or past for various tasks. This can include tasks such as setting reminders, scheduling events, or performing time-sensitive operations. In this blog post, we will explore how to easily calculate dates in the Fish Shell.

## How To
Fish Shell has a built-in function called `date` which is used for displaying the current date and time. However, we can also use this function to perform date calculations. Let's take a look at a few examples.

```Fish Shell
# To calculate a date 7 days in the future
date --date "+7 days"

# To calculate a date 3 months and 2 weeks in the past
date -d "-3 months -2 weeks"

# To calculate a specific date, such as 1st of next month
date --date "next month" "+%m/%d/%Y"
```

The first example uses the `--date` flag to specify the date format, followed by a string representing the desired time frame. In this case, we wanted to calculate 7 days in the future. The second example uses the `-d` flag to specify a date to be manipulated, followed by a string representing the desired time frame to be subtracted. In this case, we wanted to calculate 3 months and 2 weeks in the past. Lastly, the third example uses the `--date` flag to specify a specific date, and the `+%m/%d/%Y` format to display the date in the specified format.

## Deep Dive
Behind the scenes, Fish Shell uses the GNU `date` command to perform these date calculations. This command has many different options and flags that can be used to manipulate dates in various ways. You can refer to the `man` page for more information on the different options available.

One important thing to note is that the calculations are based on the system's current date and time. So if you need to perform calculations based on a different date, you may need to manipulate the system's date and time first.

## See Also
- [Fish Shell documentation on `date` function](https://fishshell.com/docs/current/cmds/date.html)
- [Man page for GNU `date` command](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Tutorial on date calculations in Bash shell](https://www.linuxjournal.com/content/normalizing-date-and-time-data-bash-shell)