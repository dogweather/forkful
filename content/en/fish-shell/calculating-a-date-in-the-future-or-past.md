---
title:    "Fish Shell recipe: Calculating a date in the future or past"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Why 

Calculating dates in the future or past can be a useful skill to have in your programming repertoire. With the ability to calculate dates, you can create automated tasks, schedule events, and more. In this blog post, we will explore how to calculate dates using the Fish Shell programming language.

## How To 

To calculate dates using the Fish Shell, we will use the `date` command. This command takes in various parameters and returns the date in a specific format. Let's take a look at some examples:

```
Fish Shell> date
Tue Sep 7 16:07:19 MST 2021
```

In this example, we simply ran the `date` command without any parameters and it returned the current date and time in the default format. But what if we want to calculate a date in the future or past? We can use the `-v` flag followed by a number and unit to specify the amount of time we want to add or subtract from the current date. Let's try adding 10 days to the current date:

```
Fish Shell> date -v 10d
Fri Sep 17 16:08:11 MST 2021
```

As you can see, the output now shows a date that is 10 days in the future. Similarly, we can also subtract time from the current date by using a negative number. Let's try subtracting 2 weeks:

```
Fish Shell> date -v -2w
Mon Aug 23 16:10:21 MST 2021
```

We can also specify the date format using the `+` flag followed by a format string. For example, if we want the output to only display the month and year, we can use the following command:

```
Fish Shell> date -v 1m +'%b %Y'
Oct 2021
```

In this example, we added one month to the current date and specified the format to only display the abbreviated month and year. There are many different format options available, so make sure to check out the `date` command documentation for more details.

## Deep Dive 

Behind the scenes, the `date` command is using the Unix timestamp to calculate the dates. A Unix timestamp is a representation of the number of seconds that have elapsed since January 1, 1970. The `date` command takes the current Unix timestamp and adds or subtracts the specified amount of time to calculate the future or past date.

You can also use the `date` command with a specific timestamp rather than the current one. This can be useful if you want to calculate dates from a certain point in time. For example, if you want to know what date will be 100 days from today, you can use the following command:

```
Fish Shell> date -v 100d -r $now
Fri Dec 17 16:19:43 MST 2021
```

In this example, we are using the `now` variable which holds the current Unix timestamp, but you can also specify a specific timestamp value.

## See Also 

- Fish Shell `date` command documentation: https://fishshell.com/docs/current/cmds/date.html
- GNU `date` command documentation: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Unix timestamp converter: https://www.unixtimestamp.com/