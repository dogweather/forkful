---
title:                "Parsing a date from a string"
html_title:           "C# recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing a date from a string is the process of converting a string containing a date into a format that can be easily manipulated by a program. This is commonly done by programmers when dealing with user input or data from external sources, such as APIs. By parsing a date, programmers can ensure that the date is in a specific format, making it easier to perform calculations or comparisons with other dates in the program.

## How to:
To parse a date from a string in C#, you can use the `DateTime.TryParse()` method. This method takes in two parameters - the string containing the date and a variable to store the parsed date. Here's an example of how to use it:

```C#
string dateString = "2019-05-12";
DateTime parsedDate;
DateTime.TryParse(dateString, out parsedDate);

Console.WriteLine("Parsed date: " + parsedDate); // Output: Parsed date: 5/12/2019 12:00:00 AM
```

You can also specify the format of the date in the string using the `DateTime.TryParseExact()` method. This is useful if the date is in a non-standard format or if the date is in a different culture. Here's an example:

```C#
string dateString = "12-May-2019";
DateTime parsedDate;
string format = "dd-MMM-yyyy";
DateTime.TryParseExact(dateString, format, CultureInfo.InvariantCulture, DateTimeStyles.None, out parsedDate);

Console.WriteLine("Parsed date: " + parsedDate); // Output: Parsed date: 5/12/2019 12:00:00 AM
```

Keep in mind that both methods return a boolean value indicating whether the parsing was successful or not. If the parsing fails, the `parsedDate` variable will be set to the default value of `DateTime`.

## Deep Dive:
Parsing dates from strings has been a common task for programmers since the early days of computing. Before the `DateTime` datatype was introduced, programmers had to manually extract the day, month, and year information from the string and convert them into integers. This was a tedious and error-prone process, hence the introduction of the `DateTime` datatype in C#.

While the `DateTime.TryParse()` method is the recommended way to parse dates in C#, there are other alternatives such as using regular expressions or third-party libraries like NodaTime. However, these alternatives may require more complex code and may not be as efficient as the built-in `DateTime` methods.

When parsing dates from strings, it's important to consider the culture and locale of the data. Different cultures may have different date formats, and using `DateTime.TryParseExact()` allows for more flexibility in handling these variations.

## See Also:
- [MSDN Documentation on DateTime.TryParse()](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tryparse)
- [MSDN Documentation on DateTime.TryParseExact()](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tryparseexact)
- [NodaTime Documentation](https://nodatime.org/)