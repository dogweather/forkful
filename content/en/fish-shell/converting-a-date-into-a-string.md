---
title:                "Fish Shell recipe: Converting a date into a string"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why
Converting a date into a string may seem like a mundane task, but it can actually be quite useful in certain situations. For example, if you need to display a date on a website or in a user interface, converting it into a string can make it more easily readable for users. Understanding how to do this in the Fish Shell can also help you become more proficient in using date and time functions in your scripts.

## How To
Converting a date into a string in the Fish Shell is a fairly simple task. Here's an example of how you can achieve this:

```Fish Shell
# Assign a date to a variable
set date (date "+%b %d, %Y")

# Convert the date into a string 
set date_string "$date"
```

In this example, we first use the `date` command to assign the current date to a variable. Then, we use the `set` command to convert the date variable into a string, which is denoted by the double quotation marks.

The output of this code would be a string in the format of "Month Day, Year" (e.g. "Oct 28, 2021").

## Deep Dive
While the conversion process may seem straightforward, there are actually a few things to consider when converting a date into a string in the Fish Shell. One important aspect is the usage of format codes in the `date` command.

Format codes allow you to format the date according to your preferences. For example, the `%b` code in our example represents the abbreviated month name, while the `%d` code represents the day of the month. You can find a comprehensive list of format codes in the Fish Shell documentation.

Another aspect to consider is the use of the `strftime` command, which allows you to format the date using a specific formatting string. This can give you even more control over the output of the date conversion.

## See Also
- [Fish Shell Documentation on Date and Time](https://fishshell.com/docs/current/commands.html#date)
- [Date Format Codes](https://fishshell.com/docs/current/commands.html#date-format-codes)
- [strftime Command Documentation](https://fishshell.com/docs/current/commands.html#strftime)