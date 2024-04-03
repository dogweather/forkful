---
date: 2024-02-03 19:02:43.902298-07:00
description: "How to: **Basic Parsing:** The `DateTime.Parse` and `DateTime.TryParse`\
  \ methods are the go-to options for converting a string into a `DateTime`. Here's\
  \ a\u2026"
lastmod: '2024-03-13T22:45:00.097833-06:00'
model: gpt-4-0125-preview
summary: '**Basic Parsing:**


  The `DateTime.Parse` and `DateTime.TryParse` methods are the go-to options for converting
  a string into a `DateTime`.'
title: Parsing a date from a string
weight: 30
---

## How to:
**Basic Parsing:**

The `DateTime.Parse` and `DateTime.TryParse` methods are the go-to options for converting a string into a `DateTime`. Here's a quick example:

```csharp
string dateString = "2023-04-12";
DateTime parsedDate;

if (DateTime.TryParse(dateString, out parsedDate))
{
    Console.WriteLine($"Successfully parsed: {parsedDate}");
}
else
{
    Console.WriteLine("Failed to parse.");
}
// Output: Successfully parsed: 4/12/2023 12:00:00 AM
```

**Specifying a Culture:**

Sometimes, you need to parse a date string that's in a specific culture format. You can achieve this using the `CultureInfo` class:

```csharp
using System.Globalization;

string dateString = "12 avril 2023";
var cultureInfo = new CultureInfo("fr-FR");
DateTime parsedDate = DateTime.Parse(dateString, cultureInfo);

Console.WriteLine(parsedDate);
// Output: 4/12/2023 12:00:00 AM
```

**Exact Parsing with a Specific Format:**

For scenarios where dates come in a specific format that might not be standard, `DateTime.ParseExact` comes in handy:

```csharp
string dateString = "Wednesday, 12 April 2023";
string format = "dddd, d MMMM yyyy";
DateTime parsedDate = DateTime.ParseExact(dateString, format, CultureInfo.InvariantCulture);

Console.WriteLine(parsedDate);
// Output: 4/12/2023 12:00:00 AM
```

**Using NodaTime:**

For even more robust date and time parsing, consider using the popular third-party library NodaTime. It provides a wider range of date/time handling capabilities:

```csharp
using NodaTime;
using NodaTime.Text;

var pattern = LocalDatePattern.CreateWithInvariantCulture("yyyy-MM-dd");
var parseResult = pattern.Parse("2023-04-12");

if (parseResult.Success)
{
    LocalDate localDate = parseResult.Value;
    Console.WriteLine(localDate); // 2023-04-12
}
else
{
    Console.WriteLine("Failed to parse.");
}
```

NodaTime offers extensive support for time zones, period and duration concepts, and many different calendar systems, making it a powerful choice for complex date and time manipulation in .NET applications.
