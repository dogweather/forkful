---
title:    "Bash recipe: Calculating a date in the future or past"
keywords: ["Bash"]
---

{{< edit_this_page >}}

# Why
In today's world, technology has made everything much easier and faster. One such task is calculating dates in the future or past. This can come in handy for various reasons, whether you want to plan for upcoming events or track historical information.

# How To
To calculate a future date or past date in Bash, we will be using the `date` command. This command is used to display or manipulate dates and times. Let's see how we can use this command to calculate dates:

```Bash
# To calculate a future date, we can use the `date` command with the `+%Y-%m-%d` option to specify the date format, and then use the `d` flag to specify the number of days we want to add.
# For example, to calculate a date one week from today:
$ date +%Y-%m-%d -d "+ 1 week"
2021-08-05

# To calculate a past date, we can use the same command but with a negative number of days.
# For example, to calculate a date one month before today:
$ date +%Y-%m-%d -d "- 1 month"
2021-06-04
```

We can also use the `-f` flag to specify a different date format, and the `-d` flag to specify a starting date. Here's an example of calculating a date in a specific format from a specific starting date:

```Bash
# To calculate a date one year from a starting date in the format "Month Day, Year":
$ date -d "2021-07-04 + 1 year" +%B\ %e,\ %Y
July 4, 2022
```

# Deep Dive
The `date` command uses Unix time, which is the number of seconds that have elapsed since January 1, 1970. When we add or subtract days, months, or years, we are essentially manipulating this time value. Therefore, careful attention must be paid to time zones and Daylight Saving Time (DST) when calculating dates.

It's also important to note that the `date` command can only calculate dates within the range of 1970-01-01 to 2038-01-19. Beyond that, it will return an error.

# See Also
Here are some useful resources for further reading and reference:

- [GNU Coreutils manual for the `date` command](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Unix time converter](https://www.unixtimestamp.com/)