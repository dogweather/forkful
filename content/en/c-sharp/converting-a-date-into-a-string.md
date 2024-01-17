---
title:                "Converting a date into a string"
html_title:           "C# recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Date conversion is the process of converting a date data type into a string data type in a programming language. This is a common practice among programmers to enable easier manipulation and display of dates in a human-readable format. It allows programmers to customize the appearance of dates to fit different languages, cultural conventions, and user preferences.

## How to:

Converting dates into strings in C# is a simple process using the `.ToString()` method. Here's an example:

```C#
DateTime date = new DateTime(2021, 08, 20);
string dateString = date.ToString("dd/MM/yyyy");
Console.WriteLine(dateString);
```

The output of this code would be: `20/08/2021`. 

The `.ToString()` method can also accept a format parameter to display the date and time in different formats. For example, using `"dd-MMM-yyyy HH:mm:ss"` as the format parameter would display the date and time as `20-Aug-2021 00:00:00`.

## Deep Dive:

Date and time are essential data types in programming, and converting them into strings is a crucial aspect of programming tasks. The need for date conversion emerged as computer systems and software became more global and needed to accommodate different date conventions. In the early days of programming, dates were often stored as integers or long numbers, making it challenging to read and manipulate them.

In C#, the `DateTime` data type represents a date and time, while the `ToString()` method is used to convert it into a string. This method accepts a format parameter that allows developers to control the appearance of the date and time. Some commonly used format parameters in C# include `"MM/dd/yyyy"` for displaying the date in the format of month/day/year, and `"dddd, dd MMMM yyyy"` for displaying the date as the day of the week, day, month, and year (e.g., Friday, 20 August 2021).

There are also other ways to convert dates into strings in C#, such as using the `DateTime.Parse()` method or using the `ToString()` method with custom format providers. These approaches may be helpful for more advanced scenarios, but for most cases, using the simple `.ToString()` method with a format parameter will suffice.

## See Also:

Microsoft C# DateTime.ToString() Method: https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tostring 

Custom Date and Time Format Strings in C#: https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings 

C# DateTime.Parse() Method: https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parse