---
title:                "Converting a date into a string"
html_title:           "Arduino recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

In C#, converting a date to a string is an operation that reformats a DateTime object into a string representation. We do this to make dates easier to process, compare, and display according to different formats and cultures.

## How to:

C# provides the `DateTime` class to handle and manipulate date and time data. Here's how you can convert a date into a string:

```C#
DateTime currentTime = DateTime.Now;
string formattedDate = currentTime.ToString("yyyy-MM-dd");
Console.WriteLine(formattedDate);
```

This code will display the current date formatted as `YYYY-MM-DD`, such as `2022-03-21`.

## Deep Dive

Historically, dates and times have been a complicated problem in programming due to various calendars used around the world. C# simplifies this with the `DateTime` and `DateTimeOffset` classes, and also the `ToString()` method, which provides multiple ways to format dates and times into strings.

There are of course alternatives to the `DateTime.ToString` method. For instance, using the `string.Format` method:

```C#
string formattedDate = string.Format("{0:yyyy-MM-dd}", DateTime.Now);
Console.WriteLine(formattedDate);
```

In terms of implementation, calling `ToString()` without arguments will give the default format specified by the system's culture. The `ToString()` method uses `CultureInfo.CurrentCulture` to format the date and time value. If you pass a format specifier, it will return a string representation based on that format.

## See Also

For further reading and examples, visit these valuable sources:

1. [`DateTime.ToString`](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tostring?view=net-5.0)
2. [`Custom date and time format strings`](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)
3. [`DateTime Structure`](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-6.0)