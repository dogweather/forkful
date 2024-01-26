---
title:                "Parsing a date from a string"
date:                  2024-01-20T15:35:09.763307-07:00
html_title:           "Arduino recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing a date from a string means converting text that represents a date into a `DateTime` object. It's crucial for saving and interpreting dates from various formats as actual dates in your code.

## How to:
```C#
using System;
using System.Globalization;

class Program
{
    static void Main()
    {
        string dateString = "2023-03-15";
        DateTime parsedDate = DateTime.Parse(dateString);
        Console.WriteLine(parsedDate); // Output: 3/15/2023 12:00:00 AM
        
        // With specific format
        dateString = "15 March, 2023";
        string format = "d MMMM, yyyy";
        CultureInfo provider = CultureInfo.InvariantCulture;
        parsedDate = DateTime.ParseExact(dateString, format, provider);
        Console.WriteLine(parsedDate); // Output: 3/15/2023 12:00:00 AM
    }
}
```

## Deep Dive
Before `DateTime`, programmers relied on custom code to handle dates, which was prone to errors and inefficiencies. The `DateTime` struct in .NET revolutionized this, providing robust parsing methods—`Parse` and `ParseExact`.

`Parse` attempts to understand a date string based on culture-specific or universal formats. Great when you expect standard date formats. However, if you have specific or unconventional date formats, `ParseExact` (along with `TryParse` and `TryParseExact` for error handling) is your friend. Here, you dictate the exact format with a custom pattern.

The implementation uses the `CultureInfo` class to respect different cultural date formats. While using `ParseExact`, you avoid cultural misunderstandings—your defined pattern is what goes. Remember, computer dates start from January 1, 0001, so make sure your string represents a valid date within the .NET calendar range.

## See Also
- [DateTime.Parse Method Documentation](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parse)
- [DateTime.ParseExact Method Documentation](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parseexact)
- [Custom Date and Time Format Strings](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [CultureInfo Class](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo)
