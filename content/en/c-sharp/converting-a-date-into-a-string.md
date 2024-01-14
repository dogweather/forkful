---
title:                "C# recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why
Have you ever needed to include a date in a string while programming in C#? Converting a date into a string can be extremely useful in many situations, such as creating a user-friendly interface or formatting data for database storage.

## How To
Converting a date into a string in C# is a simple process. First, we need to create a DateTime object with the desired date. Then, we can use the ToString() method to convert it into a string with a specific format. Let's take a look at an example:

```C#
// Create a DateTime object with the current date
DateTime today = DateTime.Today;

// Convert the date into a string using the "d" format for short date
string convertedDate = today.ToString("d");

// Output the result
Console.WriteLine(convertedDate);
// Output: 7/27/2021
```

We can also use custom formats to get the exact string representation that we need. For instance, we can use the "MMMM d, yyyy" format to get the full month name, day and year:

```C#
// Create a DateTime object with a specific date
DateTime customDate = new DateTime(2021, 12, 31);

// Convert the date into a string with a custom format
string convertedDate = customDate.ToString("MMMM d, yyyy");

// Output the result
Console.WriteLine(convertedDate);
// Output: December 31, 2021
```

There are plenty of other formatting options available, such as using the "ddd" format to get the three-letter abbreviation for the day of the week or using "hh" to get the hour in a 12-hour clock format. Make sure to check out the official Microsoft documentation for a full list of available formats.

## Deep Dive
Internally, when we convert a date into a string, the DateTime object's ToString() method uses the current culture's date and time formatting conventions. This means that the string representation of the date will vary based on the computer's regional settings. However, we can specify a specific culture in the ToString() method to get a consistent output regardless of the computer's settings.

We can also use the ParseExact() method to convert a string back into a DateTime object. This can be useful when retrieving data from a database that is stored as a string.

## See Also
- [Microsoft documentation on DateTime.ToString Method](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tostring)
- [List of DateTime formatting options](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)

Now that you know how to convert a date into a string in C#, go forth and use this skill in your future programming projects!