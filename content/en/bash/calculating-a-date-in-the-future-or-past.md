---
title:                "Bash recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to calculate a date in the future or past? Maybe you are trying to schedule an important event or keep track of important deadlines. Regardless of the reason, learning how to code a date calculator in Bash can save you time and headaches in the long run. In this blog post, we will explore the process of calculating dates using Bash programming.

## How To

To calculate a date in Bash, we will be using the `date` command. This versatile command not only displays the current date and time, but it also allows us to manipulate dates using different flags and options. Let's take a look at some examples:

```
# Calculating 10 days from today
date -d "+10 days"

# Calculating 2 weeks and 5 days from a specific date
date -d "2021-08-19 +2 weeks +5 days"

# Calculating the previous month
date -d "last month"
```

As you can see, the `date` command takes in a variety of input options, such as a specific date, a relative number of days, or even special keywords like "last month". It also lets us add or subtract days, weeks, months, or years from a given date. This makes calculating dates in Bash a breeze!

## Deep Dive

Under the hood, the `date` command uses the Unix timestamp, which is the number of seconds that have elapsed since January 1, 1970. When we specify a date, Bash converts it into a timestamp and performs calculations based on that value. This allows us to work with dates in a more precise and flexible manner.

For more complex calculations, we can also use the `expr` command to perform arithmetic operations on timestamps. This can come in handy when needing to calculate dates that are further in the future or past.

## See Also

- [Bash Date Command](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Unix Timestamp](https://en.wikipedia.org/wiki/Unix_time)
- [Bash Expr Command](https://www.gnu.org/software/coreutils/manual/html_node/expr-invocation.html)

By mastering the art of calculating dates in Bash, you can streamline your workflow and impress your peers with your programming skills. So next time you need to calculate a date in the future or past, you know exactly which language to turn to.