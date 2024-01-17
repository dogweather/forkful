---
title:                "Converting a date into a string"
html_title:           "Fish Shell recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# What & Why?

Converting a date into a string is the process of changing the format of a date to fit a specific string pattern, making it easier for programmers to manipulate and display dates in their code. This can be useful for various tasks such as sorting, filtering, and displaying dates in a user-friendly format.

# How to:

In Fish Shell, you can use the `date` command to convert a date into a string. Let's take a look at some examples:

```
Fish Shell > date "+%Y-%m-%d"
2021-04-30
```

In this example, we are using the `date` command with the `+%Y-%m-%d` option to convert the current date to the format of year-month-day. The output will vary depending on the date when you run this command.

You can also specify a specific date to convert, instead of using the current date. For example:

```
Fish Shell > date -d "yesterday" "+%A, %B %d, %Y"
Thursday, April 29, 2021
```

This command uses the `-d` option to specify a date (in this case, "yesterday") and the `+%A, %B %d, %Y` option to convert it to the format of day, month, date, and year.

# Deep Dive

Converting a date into a string is a common task in programming, especially when dealing with data that includes dates. In the past, it was a more complicated process, requiring manual conversions and calculations. However, with the `date` command in Fish Shell, it has become much simpler and more efficient.

There are alternative ways to convert a date into a string, such as using built-in functions in other languages like Python or JavaScript. However, Fish Shell's `date` command is a handy tool for quick conversions without the need for additional dependencies.

For those interested in the technical details, the `date` command in Fish Shell uses the `strftime` function, which is a C library function that converts a date into a specified string format.

# See Also

- [Fish Shell documentation for the `date` command](https://fishshell.com/docs/current/cmds/date.html)
- [strftime function documentation](https://www.gnu.org/software/libc/manual/html_node/Formatting-Calendar-Time.html#Formatting-Calendar-Time)