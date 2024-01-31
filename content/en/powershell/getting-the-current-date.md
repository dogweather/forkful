---
title:                "Getting the current date"
date:                  2024-01-20T15:15:52.998123-07:00
simple_title:         "Getting the current date"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?

Grabbing the current date in PowerShell is just fetching the system's idea of today's date. Coders use this to timestamp logs, calculate time spans, or trigger time-specific operations.

## How to:

Here's the straight-up code to snag today's date:

```PowerShell
Get-Date
```

And voila, output:

```plaintext
Tuesday, March 14, 2023 10:15:42 AM
```

Maybe you want something more specific, like just the day:

```PowerShell
(Get-Date).Day
```
Output:

```plaintext
14
```

How about we go international? Get the date in ISO 8601 format:

```PowerShell
Get-Date -Format 'yyyy-MM-dd'
```

Output:

```plaintext
2023-03-14
```

## Deep Dive

Back in the day, getting the date in scripting languages was no trivial matter. But PowerShell, having learned from the complexities and necessities of computing history, made it a one-liner.

Beyond `Get-Date`, alternatives include diving into the .NET System.DateTime class for more complex needs, or using WMI (Windows Management Instrumentation) to fetch system info. Still, `Get-Date` is the go-to for simplicity and effectiveness.

Under the hood, `Get-Date` taps into your system's clock and regional settings to ensure the date and time are accurately represented, reflective of time zones and daylight saving adjustments.

It's also fully customizable. You can mold the output format to your heart's content using standard or custom format strings - a handy feature for logs that need to follow certain conventions or for visual harmony in your output.

## See Also

Here are a few resources to check out:

- [Get-Date documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)
- [.NET DateTime structure](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
- [Custom date and time format strings](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)
