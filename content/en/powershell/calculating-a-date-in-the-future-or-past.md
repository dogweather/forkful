---
title:                "Calculating a date in the future or past"
html_title:           "PowerShell recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Calculating a Date in the Future or Past with PowerShell

## What & Why?

Manipulating dates is a common operation where we calculate a date in the future or past from a given date. This helps in scheduling tasks, performing chronological comparison, or generating date-dependent reports.

## How to:

PowerShell makes manipulating dates incredibly easy. Below are some simple examples:

```PowerShell
# To get the current date
$today = Get-Date
$today
```

The output will be the current system date.

To add or subtract days, hours, minutes, or seconds from the current date, you can use the AddDays(), AddHours(), AddMinutes(), and AddSeconds() methods.

```PowerShell
# To get the date 7 days from today
$sevenDaysFromNow = $today.AddDays(7)
$sevenDaysFromNow
```

The output will be the date for 7 days in the future. 

Using similar concept, you can get a date in the past.

```PowerShell
# To get the date 30 days ago
$thirtyDaysAgo = $today.AddDays(-30)
$thirtyDaysAgo
```

The output will be the date 30 days ago from the current date.

## Deep Dive

Historically, dealing with date and time was a big challenge in most programming languages. With the introduction of .NET framework and subsequently PowerShell (which is built on .NET), tasks involving date and time are significantly eased.  

PowerShell offers alternative ways to manipulate dates. For example, the New-TimeSpan cmdlet can be used to get a time difference and then add or subtract the result from a date.

```PowerShell
$oneWeek = New-TimeSpan -Days 7
$futureDate = $today + $oneWeek
$futureDate
```

Under the hood, these functions work on the DateTime object of the .NET Framework. This object stores date and time data, offering numerous methods for date/time manipulation. 

## See Also

1. For more information on date and time in PowerShell, see the [official documentation](https://docs.microsoft.com/powershell/scripting/samples/working-with-dates-and-times?view=powershell-7.1).
   
2. To understand more about .NET DateTime, refer to the [.NET documentation](https://docs.microsoft.com/dotnet/api/system.datetime?view=net-5.0). 

3. For an in-depth discussion on time manipulation in PowerShell, visit [ITPro Today](https://www.itprotoday.com/powershell/date-and-time-math-powershell).