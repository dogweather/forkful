---
title:                "Comparing two dates"
html_title:           "PowerShell recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?
Comparing two dates is the process of determining if two given dates are the same, earlier, or later. This is a common task for programmers when working with time-sensitive data. By comparing dates, programmers can easily sort and classify data based on their chronological order.

## How to:
Comparing two dates in PowerShell is a straightforward process. First, we need to create two date objects using the ```Get-Date``` cmdlet. Then, we can use the comparison operators - ```-eq``` (equal to), ```-gt``` (greater than), and ```-lt``` (less than) to compare the two dates.

```PowerShell
$firstDate = Get-Date "10/15/2021"
$secondDate = Get-Date "10/20/2021"

$firstDate -eq $secondDate #output: False
$firstDate -gt $secondDate #output: False
$firstDate -lt $secondDate #output: True
```

In the above example, we create two dates - October 15th, 2021 and October 20th, 2021. By using the ```-eq```, ```-gt```, and ```-lt``` operators, we can compare the two dates and receive a Boolean output indicating the result of the comparison.

It is also possible to use the ```-eq``` operator with the ```Compare-Object``` cmdlet to compare multiple dates at once. 

```PowerShell
$dates = @("10/15/2021", "10/17/2021", "10/20/2021")
$referenceDate = Get-Date "10/18/2021"

Compare-Object $dates $referenceDate -IncludeEqual #output: 10/17/2021
```

In this example, we create an array of dates and use the ```Compare-Object``` cmdlet to compare them to a reference date. By using the ```-IncludeEqual``` parameter, we can also include the dates that are equal to the reference date in the output.

## Deep Dive:
Historically, date comparisons were done by converting dates into numeric values and then comparing them. However, this method was not reliable as it did not account for different date formats and cultures. With PowerShell, we can now use the built-in ```Get-Date``` cmdlet to create date objects and use the comparison operators for accurate comparisons.

Other alternative methods for comparing dates in PowerShell include using the ```New-TimeSpan``` cmdlet to calculate the difference between two dates and using the date formatting options to ensure consistency in comparisons.

When comparing dates in PowerShell, it is essential to pay attention to the format of the dates. Different cultures and regions may use different date formats, so it is crucial to consider this when writing scripts that will be used globally.

## See Also:
- [PowerShell documentation on Get-Date](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)
- [PowerShell documentation on Compare-Object](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/compare-object?view=powershell-7.1)
- [Alternative methods for comparing dates in PowerShell](https://adamtheautomator.com/powershell-compare-dates/)