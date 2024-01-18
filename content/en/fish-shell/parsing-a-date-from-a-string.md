---
title:                "Parsing a date from a string"
html_title:           "Fish Shell recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing a date from a string is the process of extracting a specific date value from a given string. This can be useful for programmers who need to work with date data in their code, such as converting a string input into a more usable format or performing calculations based on specific dates.

## How to:

The Fish Shell, like many other programming languages, has built-in functions that allow for easy parsing of dates from strings. Here are a few examples of how to use them:

```
Fish Shell 2.7.1
```
### Parsing a date from a string in ISO format
```
> set my_date (date -f %Y-%m-%d "2022-01-01")
> echo $my_date
January 1, 2022
```

### Parsing a date from a custom string
```
> set custom_date (date -f "I was born on %B %d, %Y" "I was born on March 12, 1990")
> echo $custom_date
March 12, 1990
```

### Converting a parsed date into a specific format
```
> set original_date "03/12/2020"
> set converted_date (date -f "%B %d, %Y" $original_date)
> echo $converted_date
March 12, 2020
```

## Deep Dive

Parsing dates from strings has been a common task for many programmers since the early days of computing. In those days, it was often a tedious and error-prone process, requiring manual string manipulations and conversions. However, with the advancement of programming languages and tools, it has become a much simpler and more streamlined process.

Besides using built-in functions in programming languages, there are also alternative ways to parse dates from strings, such as using regular expressions or third-party libraries and modules. However, the built-in functions provided by Fish Shell are usually more efficient and reliable.

As for the implementation details, parsing a date from a string usually involves identifying and extracting the relevant date components (such as day, month, and year) from the input string and converting them into a standardized format. This process can become more complex when dealing with different date formats or when incorporating localization for different languages.

## See Also

If you're interested in learning more about parsing dates from strings, here are some helpful resources:

- [Fish Shell documentation](https://fishshell.com/docs/current/cmds/date.html) for more information on the `date` command and its options.
- [ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html) standard for date and time representation.
- [MDN web docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date) for date-related functions and methods in JavaScript, a commonly used programming language for web development.