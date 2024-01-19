---
title:                "Converting a date into a string"
html_title:           "Arduino recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# An Uncomplicated Guide to Converting Date to String in Fish Shell

## What & Why?

Converting a date to a string involves transforming a date object into a written or printable form. Programmers do this to make dates human-readable and suitable for displaying or processing further.

## How to:

In Fish Shell, you'd commonly use the built-in `date` command to convert a date into a string.

Here is how to get the current date in a commonly human-readable format:

```Fish Shell
echo (date "+%m-%d-%Y")
```

The output would look something like this representing, month-day-year:

```Fish Shell
09-14-2022
``` 

If you wish to name your weekdays, use `%A` for the full day name, or `%a` for abbreviated. Let's try:

```Fish Shell
echo (date "+%A, %m-%d-%Y")
```

The output will look like this:

```Fish Shell
Wednesday, 09-14-2022
```

## Deep Dive:
   
Computers store dates as numerical formats. The Unix timestamp for example, represents the number of seconds elapsed since January 1, 1970. While this is great for computations, it's not very human-friendly. String representation of dates existed back when the Unix `date` command was created in the early 1970s.

Alternative ways for date representation include using external utilities or programming languages. Python, for example, provides extensive date manipulation capabilities with its built-in `datetime` library. 

In Fish Shell, `date` command default output format depends on the system locale. But by using formatting options, like `%m` for month, `%d` for day, `%Y` for year, you have control over the string output.

## See Also:

Here's where you can dive deeper:

- Fish Shell documentation: [https://fishshell.com/docs/current/](https://fishshell.com/docs/current/)
- `date` man page: [https://linux.die.net/man/1/date](https://linux.die.net/man/1/date)
- Python `datetime` library: [https://docs.python.org/3/library/datetime.html](https://docs.python.org/3/library/datetime.html)