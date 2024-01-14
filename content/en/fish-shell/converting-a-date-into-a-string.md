---
title:    "Fish Shell recipe: Converting a date into a string"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why
Converting a date into a string is a crucial task in programming as it allows us to format dates according to our needs. This can range from displaying the date in a specific format, to comparing and manipulating dates. In this blog post, we will explore how to convert a date into a string using Fish Shell programming.

## How To
To convert a date into a string using Fish Shell, we will be using the `date` command. This command can be used to format the current date, or a specified date, into a string according to a desired format.

```
Fish Shell:

# Store current date in a variable
set currentDate (date)

# Convert current date into string
echo $currentDate

# Output: Thu May 27 04:18:07 UTC 2021

# Convert into ISO date format
echo $currentDate | date --iso

# Output: 2021-05-27
```

In the above example, we first store the current date in a variable using the `date` command. Then, we use the `echo` command to print the value of the variable which gives us the current date in string format. Next, we use the `|` symbol to pipe the output of `echo` into the `date` command, specifying the `--iso` option to format the date in ISO format.

Fish Shell also allows us to specify a custom format for the date using the `--format` option. Let's see an example of that:

```
Fish Shell:

# Convert into custom date format
echo $currentDate | date --format "%d/%m/%Y"

# Output: 27/05/2021
```

In this example, we have specified the format as `%d/%m/%Y` which translates to day/month/year. This allows us to customize the date format according to our needs.

## Deep Dive
Behind the scenes, the `date` command is using the `strftime` function from the C library to format the date. The format specifiers used in the `--format` option are derived from the `strftime` function. This allows us to have more control over the output of the formatted date.

Some commonly used format specifiers are:
- %d: day of the month (01-31)
- %m: month of the year (01-12)
- %Y: year (e.g. 2021)
- %H: hour in 24-hour format (00-23)
- %M: minute (00-59)
- %S: seconds (00-59)
- %a: abbreviated weekday (e.g. Thu)
- %b: abbreviated month (e.g. May)

For a complete list of format specifiers, you can refer to the [`strftime` documentation](http://www.cplusplus.com/reference/ctime/strftime/).

## See Also
- [Fish Shell `date` command documentation](https://fishshell.com/docs/current/cmds/date.html)
- [`strftime` documentation](http://www.cplusplus.com/reference/ctime/strftime/)