---
title:                "Fish Shell recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why
Converting dates into strings is a common task in programming, especially when working with user input or data from databases. It allows for easier manipulation and organization of dates, making them more user-friendly and potentially easier to work with in your code.

## How To

Fish Shell provides a simple and efficient way to convert dates into strings. Let's consider the following example code:

```
Fish Shell date '%Y-%m-%d'
```
This will output the current date in the format of "year-month-day". You can also specify a specific date like this:

```
Fish Shell date -f '%Y-%m-%d' '1984-06-21'
```
This will convert the date "1984-06-21" into the format "year-month-day". 

You can also add more complexity by including time in the string, like this:

```
Fish Shell date -f '%Y-%m-%d %H:%M' '2021-09-01 13:45'
```

This will print out the date and time in the format "year-month-day hour:minute".

## Deep Dive
Fish Shell uses the date command, which allows for a variety of formatting options. Some common options include:

- %Y: 4-digit year
- %m: 2-digit month
- %d: 2-digit day
- %H: 24-hour format hour
- %M: 2-digit minute

You can also combine these options to create a custom date string that fits your specific needs. It's important to note that the syntax for the date command may vary slightly depending on your operating system.

## See Also
- [Fish Shell date documentation](https://fishshell.com/docs/current/commands.html#date)
- [GNU date manual](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Stack Overflow thread on converting dates to strings](https://stackoverflow.com/questions/18305239/how-to-convert-a-date-into-a-string)