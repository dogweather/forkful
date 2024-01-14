---
title:    "Fish Shell recipe: Getting the current date"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Why
In this blog post, we'll be discussing how to get the current date using Fish Shell. By knowing how to do this, you can easily keep track of what day it is and use it in your scripting tasks.

## How To
To get the current date in Fish Shell, we'll be using the `date` command. This command can display the current date in various formats depending on the options passed to it. Let's take a look at some examples:

```Fish Shell
# Display the date in the default format
$ date
Mon Jul 29 14:56:40 EDT 2019

# Display the date in ISO-8601 format
$ date -I
2019-07-29

# Display only the current year
$ date +%Y
2019

# Display only the current month
$ date +%m
07

# Display only the current day
$ date +%d
29

# Display the date in a custom format
$ date +"%A, %B %d, %Y"
Monday, July 29, 2019
```

As you can see, the `date` command is quite versatile and can be used in various ways to get the current date. You can also use it in your scripts to dynamically generate file names or timestamps.

## Deep Dive
For those who want to dive deeper into getting the current date in Fish Shell, it's worth mentioning that the `date` command is actually a wrapper around the `strftime()` function. This function is used to format dates and times in different ways and is available in many programming languages.

You can also use the `date` command to display the date in different time zones by passing the `-u` flag followed by a specific time zone, such as `GMT` or `UTC`.

For more information about the `date` command and its options, you can use the built-in `help` command in Fish Shell or refer to the [official documentation](https://fishshell.com/docs/current/cmds/date.html).

## See Also
- [Fish Shell documentation on `date` command](https://fishshell.com/docs/current/cmds/date.html)
- [strftime() function documentation](https://www.cplusplus.com/reference/ctime/strftime/)
- [List of time zone abbreviations](https://en.wikipedia.org/wiki/List_of_tz_database_time_zones)