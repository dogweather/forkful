---
title:                "Getting the current date"
html_title:           "PowerShell recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?

Getting the current date is a basic task that programmers often have to do in their day-to-day work. It simply refers to retrieving the current date and time from the system's clock. This information can be useful in various applications, such as creating time-sensitive tasks or displaying the current date on a user interface.

## How to:

To get the current date in PowerShell, you can use the Get-Date cmdlet. This cmdlet retrieves the current date and time and displays it in the default format. Here's an example of how to use it:

```PowerShell
Get-Date
```

The output will look something like this:

```
Thursday, September 23, 2021 2:30:00 PM
```

If you want to change the format of the date, you can use the -Format parameter with the Get-Date cmdlet. For example:

```PowerShell
Get-Date -Format "MM/dd/yyyy"
```

This will display the date in the mm/dd/yyyy format, like this:

```
09/23/2021
```

You can also use the [DateTime] class to get the current date. This is useful when you need to format the date in a specific way. Here's an example:

```PowerShell
[DateTime]::Now.ToString("yyyy-MM-dd")
```

This will give you the current date in the yyyy-MM-dd format, like this:

```
2021-09-23
```

## Deep Dive:

Historically, getting the current date involved using the Date function in older programming languages like BASIC or COBOL. With the rise of object-oriented programming, languages like Java and C# introduced classes and methods specifically for handling date and time.

In PowerShell, you can also use the DateTime class to get the current date, as shown in the previous section. However, the Get-Date cmdlet is a more convenient and user-friendly option. 

For alternative ways to get the current date in PowerShell, you can also use the .NET Framework's System.DateTime class or the WMI (Windows Management Instrumentation) class, Win32_LocalTime.

## See Also:

- [Get-Date cmdlet documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date)
- [Working with dates and times in PowerShell](https://adamtheautomator.com/working-with-dates-and-times-in-powershell/)