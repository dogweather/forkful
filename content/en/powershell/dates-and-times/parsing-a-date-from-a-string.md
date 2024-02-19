---
aliases:
- /en/powershell/parsing-a-date-from-a-string/
date: 2024-02-03 19:02:46.575014-07:00
description: "Parsing a date from a string is about recognizing and converting written\
  \ dates in text form into a date data type that PowerShell can understand and work\u2026"
lastmod: 2024-02-18 23:09:11.283374
model: gpt-4-0125-preview
summary: "Parsing a date from a string is about recognizing and converting written\
  \ dates in text form into a date data type that PowerShell can understand and work\u2026"
title: Parsing a date from a string
---

{{< edit_this_page >}}

## What & Why?
Parsing a date from a string is about recognizing and converting written dates in text form into a date data type that PowerShell can understand and work with. Programmers do this to manipulate, format, compare, or calculate dates, which are common tasks in scripts dealing with log files, user input, or data processing.

## How to:
PowerShell makes parsing dates from strings straightforward with its `Get-Date` cmdlet and `[datetime]` type accelerator, which work well for standard date formats. For more complex or non-standard date strings, the `[datetime]::ParseExact` method can be utilized to specify the exact format.

### Using `Get-Date` and `[datetime]`:
```powershell
# Simple conversion using Get-Date
$stringDate = "2023-04-01"
$date = Get-Date $stringDate
echo $date
```
**Sample output:**
```
Saturday, April 1, 2023 12:00:00 AM
```

```powershell
# Using the type accelerator [datetime]
$stringDate = "April 1, 2023"
$date = [datetime]$stringDate
echo $date
```
**Sample output:**
```
Saturday, April 1, 2023 12:00:00 AM
```

### Using `[datetime]::ParseExact` for non-standard formats:
For formats not automatically recognized, you can define the exact format to ensure correct parsing.
```powershell
$stringDate = "01-04-2023 14:00"
$format = "dd-MM-yyyy HH:mm"
$culture = [Globalization.CultureInfo]::InvariantCulture
$date = [datetime]::ParseExact($stringDate, $format, $culture)
echo $date
```
**Sample output:**
```
Saturday, April 1, 2023 2:00:00 PM
```

### Leveraging Third-party Libraries
Although PowerShell itself is quite powerful for date parsing, for very complex scenarios or additional functionality, you might explore .NET libraries such as NodaTime, though for many typical use cases, PowerShell's native capabilities will suffice.

```powershell
# Using NodaTime just as an illustration, note you need to add the library to your project
# Install-Package NodaTime -Version 3.0.5
# Using NodaTime to parse a date
[string]$stringDate = "2023-04-01T14:00:00Z"
[NodaTime.Instant]::FromDateTimeUtc([datetime]::UtcNow)
[NodaTime.LocalDate]$localDate = [NodaTime.LocalDate]::FromDateTime([datetime]::UtcNow)
echo $localDate
```
**Sample Note:** The above code is a conceptual illustration. In practice, ensure NodaTime is correctly added to your project for the types and methods to be available.
