---
title:    "Bash recipe: Converting a date into a string"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why
Sometimes in Bash programming, we may need to convert a date into a string for various purposes. This could be for displaying the date in a specific format, manipulating the date data, or passing it as an argument to another function. Whatever the reason may be, understanding how to convert a date into a string can be a useful skill for any Bash programmer.

## How To

To convert a date into a string, we will be using the `date` command in Bash. This command allows us to manipulate and format date and time information. In this case, we will be using it to convert a date into a string in a specific format.

Let's take a look at an example:

```Bash
# Get current date
current_date=$(date +"%Y-%m-%d")
# Print date as a string
echo "Today's date is $current_date"
```

In this example, we are using the `date` command with the `+%Y-%m-%d` option to get the current date in the format of "year-month-day". We then store this value into a variable called `current_date` and use it to print the date as a string.

The output of this code would look something like this:

```
Today's date is 2021-10-06
```

This is just one example of how we can convert a date into a string in Bash. The `date` command offers many other options to format the date and time in any way we want.

## Deep Dive

When using the `date` command, we can use a combination of different options to customize the output format according to our needs. Some common options that can be useful for converting a date into a string are:

- `%Y` - for the year in four digits (e.g. 2021)
- `%m` - for the month in two digits (e.g. 10)
- `%d` - for the day in two digits (e.g. 06)
- `%H` - for the hour in 24-hour format (e.g. 14)
- `%M` - for the minute in two digits (e.g. 30)
- `%S` - for the second in two digits (e.g. 45)

We can also add other characters between these options to create a custom format. For example, if we want the date and time in the format of "Month DD, YYYY - HH:MM:SS", we can use the following command:

```Bash
# Get current date and time
current_date=$(date +"%B %d, %Y - %H:%M:%S")
# Print date and time as a string
echo "Current date and time is $current_date"
```

And the output would be:

```
Current date and time is October 06, 2021 - 14:30:45
```

By understanding the various options available with the `date` command, we can create any date and time format we need and convert it into a string in Bash.

## See Also

- [Bash Date Command](https://www.computerhope.com/unix/bash/date.htm)
- [Converting Date Formats in Bash](https://linuxize.com/post/bash-date-command/)
- [Date Man Page](https://man7.org/linux/man-pages/man1/date.1.html)