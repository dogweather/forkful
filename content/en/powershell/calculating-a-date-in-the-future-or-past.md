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

## What & Why?

Calculating a date in the future or past is the task of determining a specific date, either ahead or behind the current date, based on a given time period or interval. This is commonly done by programmers in order to automate tasks or schedule events.

## How to:

Calculating a date in the future or past in PowerShell is simple and can be done in various ways.

First, we can use the `AddDays()` method to add a specific number of days to the current date. For example, to calculate a date 5 days in the future, we can use the following code:

```PowerShell
(Get-Date).AddDays(5)
```

The output would be the date 5 days from the current date.

We can also use the `AddMonths()` method to add a specific number of months to the current date. For example, to calculate a date 3 months in the future, we can use the following code:

```PowerShell
(Get-Date).AddMonths(3)
```

Finally, we can use the `AddYears()` method to add a specific number of years to the current date. For example, to calculate a date 2 years in the future, we can use the following code:

```PowerShell
(Get-Date).AddYears(2)
```

## Deep Dive:

Historically, calculating dates in the future or past has been done manually or with the use of complex algorithms. However, with the advancements in technology and programming languages, this task has become much simpler and can be done with just a few lines of code.

In addition to the methods mentioned above, PowerShell also has other built-in methods such as `AddHours()`, `AddMinutes()`, and `AddSeconds()` which can be used to add a specific amount of time to the current date.

An alternative method to calculate a date in the future or past is by using the `New-TimeSpan` cmdlet. This cmdlet allows us to specify a start date and a time interval in order to calculate the end date. For example, to calculate a date 10 days in the future, we can use the following code:

```PowerShell
$endDate = (Get-Date) + New-TimeSpan -Days 10
```

## See Also:

For more information on calculating dates in PowerShell, you can refer to the official documentation: https://docs.microsoft.com/en-us/sql/powershell/sql-server-pester-testing?view=sql-server-ver15.

Additionally, you can also check out these related sources:

- https://www.petri.com/powershell-problem-solver-calculate-future-dates
- https://docs.microsoft.com/en-us/powershell/scripting/overview?view=powershell-7.1