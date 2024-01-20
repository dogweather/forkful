---
title:                "Comparing two dates"
html_title:           "Elm recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?

Comparing two dates is about determining whether one date is earlier, equal, or later than another. Programmers do this to make decisions, execute relevant logic, or track time-sensitive data in their applications. 

## How to:

Let's see how you can compare two dates in PowerShell. This code block compares two manually set dates:

```PowerShell
$date1 = Get-Date -Year 2021 -Month 7 -Day 11
$date2 = Get-Date -Year 2021 -Month 12 -Day 30

if ($date1 -gt $date2)
{
    Write-Output "$date1 is later than $date2"
}
elseif ($date1 -eq $date2)
{
    Write-Output "$date1 is the same as $date2"
}
else
{
    Write-Output "$date1 is earlier than $date2"
}
```
Sample output: `Sunday, July 11, 2021 12:00:00 AM is earlier than Thursday, December 30, 2021 12:00:00 AM.`

## Deep Dive

PowerShell uses .NET’s DateTime object to manipulate dates and times, which has been around since the first .NET framework release in 2002. Thence, `Get-Date` command serves to return a DateTime object.

There exists alternatives to `-gt` (greater than) and `-lt` (less than) operators, such as `.CompareTo()` method. However, such alternatives prove less friendly in PowerShell scripts, and the aforementioned operators tend to be more idiomatic.

Remember, PowerShell relies on the system's timezone for date comparison unless otherwise specified. Always check your machine's timezone settings when working with date comparison involving user input or data fetched from different timezones.

## See Also

- [Microsoft's Official Documentation on Get-Date](https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/get-date)