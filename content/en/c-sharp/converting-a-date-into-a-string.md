---
title:    "C# recipe: Converting a date into a string"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

There are many instances where you may need to convert a date into a string while working on a project in C#. This can be for displaying the date in a specific format, storing it in a database, or passing it as a parameter to an API request. Whatever the reason may be, knowing how to convert a date into a string is a handy skill to have in your programming arsenal.

## How To

To convert a date into a string in C#, you can use the `ToString()` method and specify the desired format using a format string. Let's look at an example:

```C#
DateTime currentDate = DateTime.Now;
string formattedDate = currentDate.ToString("dd/MM/yyyy");
Console.WriteLine(formattedDate);
```

The output of this code would be the current date in the format "DD/MM/YYYY". In this example, we used the `ToString()` method and passed in a format string consisting of "dd/MM/yyyy". The `dd` represents the day, `MM` represents the month, and `yyyy` represents the year. You can use different combinations of these format specifiers to achieve your desired format.

You can also use the `ToString()` method to convert a date into a string in a specific culture. For example, if you want to display the date in French culture, you can use the `CultureInfo` class and pass it as a parameter to the `ToString()` method. Here's an example:

```C#
// Using French culture
CultureInfo frenchCulture = CultureInfo.CreateSpecificCulture("fr-FR");
string formattedDate = currentDate.ToString("dd/MM/yyyy", frenchCulture);
Console.WriteLine(formattedDate);
```

The output of this code would be the current date displayed in French format instead of the default English format.

## Deep Dive

Under the hood, the `ToString()` method uses the `IFormattable` interface to convert a date into a string. This interface contains a `ToString` method that accepts a format string and culture as parameters. When you call the `ToString()` method on a date object, it internally calls the `ToString()` method of the `IFormattable` interface with default parameters. In the first example, we passed in a format string, and in the second example, we passed in a culture, but you can also pass in both parameters to the `ToString()` method to achieve more precise formatting of your string.

Apart from using the `ToString()` method, you can also make use of `DateTime` class' `ToString(string format)` method, which provides a more straightforward way to specify a date format. Let's look at an example:

```C#
// Using the "d" format specifier
string formattedDate = currentDate.ToString("d");
Console.WriteLine(formattedDate);
```

The `d` format specifier represents the short date pattern in the current culture. You can find a list of all the available format specifiers and their meanings on Microsoft's [Custom Date and Time Format Strings](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings) documentation.

## See Also
- [DateTime.ToString Method (System)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tostring?view=net-5.0)
- [CultureInfo Class (System.Globalization)](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo?view=net-5.0)
- [Custom Date and Time Format Strings (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)