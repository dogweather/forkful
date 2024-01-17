---
title:                "Getting the current date"
html_title:           "C# recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?

In C#, as well as in many other programming languages, getting the current date and time is a common task. This function allows programmers to include dynamic and up-to-date information in their programs, such as timestamps or time-related calculations. 

## How to:

To get the current date and time in C#, you can use the `DateTime.Now` property. This will return a `DateTime` object with the current date and time. See the example below:

```C#
DateTime currentDate = DateTime.Now;
Console.WriteLine(currentDate);
```

Output:
```
8/27/2021 2:30:56 PM
```

You can also format the output using the `ToString()` method and specifying a format string. For example:

```C#
DateTime currentDate = DateTime.Now;
Console.WriteLine(currentDate.ToString("MM/dd/yyyy"));
```

Output:
```
08/27/2021
```

## Deep Dive:

Before the introduction of `DateTime` in .NET Framework 1.0, managing date and time in applications was a complex and error-prone task. The `DateTime` class provides a simple and reliable way to work with dates and times in C#.

An alternative way to get the current date and time is by using the `DateTime.UtcNow` property, which returns the current date and time in UTC format. This can be useful for applications that need to be time-zone independent.

The implementation of `DateTime.Now` uses the system clock to retrieve the current date and time. This means that if the system clock is changed, the returned value will also change.

## See Also:

- [DateTime.Now Property - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.now?view=net-5.0)
- [Working with Dates and Times in C# - codeburst](https://codeburst.io/working-with-dates-and-times-in-c-599ba50ee9d3)
- [DateTimeOffset Structure - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.datetimeoffset?view=net-5.0)