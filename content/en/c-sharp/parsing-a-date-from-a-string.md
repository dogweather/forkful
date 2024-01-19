---
title:                "Parsing a date from a string"
html_title:           "C recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Parsing a Date from a String in C#

## What & Why?
Parsing a date from a string involves converting the text representation of a date and time to its DateTime equivalent. Programmers do it when dealing with DateTime values that are user-inputted or extracted from a text file, a webpage, etc. 

## How To:

Use the DateTime.Parse, DateTime.TryParse, DateTime.ParseExact or DateTime.TryParseExact methods from the .NET library. A basic example using DateTime.Parse is:

```C#
string dateInput = "06/15/2021 13:45";  
DateTime parsedDate;  
parsedDate = DateTime.Parse(dateInput);  
Console.WriteLine(parsedDate);
```

When you run this code, the output will be: 

`6/15/2021 1:45:00 PM`

If you're dealing with a date in a specific format, DateTime.ParseExact is your go-to method. Here's an example:

```C#
string dateInput = "15/06/2021";
string dateFormat = "dd/MM/yyyy";
DateTime parsedDate;
parsedDate = DateTime.ParseExact(dateInput, dateFormat, null);  
Console.WriteLine(parsedDate);
```

Running this snippet yields:

`6/15/2021 12:00:00 AM`

## Deep Dive

Historically, programmers used custom code to parse a date from a string, leading to inconsistencies and bugs. Furthering the .NET framework addressed this problem with standardized parsing methods, fostering code uniformity.

Concerning alternatives to .NET parsing methods, other programming languages have their mechanisms. For example, Python provides the datetime library and the strptime function to parse dates.

Regarding C# implementation, DateTime.Parse, and DateTime.TryParse adopt the current culture's date format, while DateTime.ParseExact and DateTime.TryParseExact allow specification of the exact format.

## See Also

In-depth documentation of DateTime.Parse, DateTime.TryParse, DateTime.ParseExact, and DateTime.TryParseExact is available on Microsoft's .NET Documentation at:

- [DateTime.Parse Method](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parse?view=net-5.0)
- [DateTime.TryParse Method](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tryparse?view=net-5.0)
- [DateTime.ParseExact Method](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parseexact?view=net-5.0)
- [DateTime.TryParseExact Method](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tryparseexact?view=net-5.0)