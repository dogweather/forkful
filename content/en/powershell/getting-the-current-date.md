---
title:                "Getting the current date"
html_title:           "Elm recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?

Getting the current date in PowerShell is retrieving the present day, month, and year data. Programmers do it to log events, timestamp documents, or compare dates for various application needs.

## How to:

Get the current date using the `Get-Date` cmdlet as seen in the block below:

```PowerShell 
$currentDate = Get-date
$currentDate
```

It will display the present date including the time:

```PowerShell 
Thursday, March 10, 2022 11:05:25 PM
```

If you want just the date, format it this way:

```PowerShell 
$currentDate = (Get-Date).ToString('MM/dd/yyyy')
$currentDate
```

Your output will look like this, depending on the present day:

```PowerShell 
03/10/2022
```

## Deep Dive

Historically, PowerShell derived the `Get-Date` cmdlet from .NET's `DateTime` objects, hence the almost similar functionalities. Different methods of `DateTime` objects can be applied to it.

Alternatives to `Get-date` include `.NET classes` directly:

```PowerShell
[System.DateTime]::Now
```

A fun fact with PowerShell getting the current date is its ability to retrieve it in many ways. Not just 'now' but 'yesterday', 'last month', 'next week', etc. with `Get-Date`. It also easily formats dates, crucial for developers dealing with data presentation.

## See Also

For more details, you can check out these resources:
1. Get-Date cmdlet:[Microsoft Documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.2)
3. .NET DateTime methods: [.NET API Browser](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-6.0)