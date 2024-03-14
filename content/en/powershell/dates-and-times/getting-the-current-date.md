---
date: 2024-02-03 19:02:40.928755-07:00
description: "Retrieving the current date in PowerShell is about fetching the system\u2019\
  s current date and time. This operation is fundamental for tasks such as logging,\u2026"
lastmod: '2024-03-13T22:45:00.293854-06:00'
model: gpt-4-0125-preview
summary: "Retrieving the current date in PowerShell is about fetching the system\u2019\
  s current date and time. This operation is fundamental for tasks such as logging,\u2026"
title: Getting the current date
---

{{< edit_this_page >}}

## What & Why?

Retrieving the current date in PowerShell is about fetching the system’s current date and time. This operation is fundamental for tasks such as logging, timing operations, or making decisions based on dates. Programmers use this capability to track events, schedule tasks, and handle date-specific logic in scripts and applications.

## How to:

PowerShell provides straightforward cmdlets for getting the date and time. The `Get-Date` cmdlet is the primary tool for this purpose. It can return the full date and time, which you can format or manipulate according to your needs.

```powershell
# Get the current date and time
Get-Date
```

**Sample output:**

```
Tuesday, September 5, 2023 9:46:02 AM
```

You can also format the output to display only the information you need, such as just the date or just the time.

```powershell
# Get only the current date in a specific format
Get-Date -Format "yyyy-MM-dd"
```

**Sample output:**

```
2023-09-05
```

```powershell
# Get only the current time
Get-Date -Format "HH:mm:ss"
```

**Sample output:**

```
09:46:02
```

### Using .NET Class

PowerShell allows direct access to .NET classes, offering an alternative way to work with dates and times.

```powershell
# Using .NET DateTime class to get the current date and time
[System.DateTime]::Now
```

**Sample output:**

```
Tuesday, September 5, 2023 9:46:02 AM
```

For UTC time:

```powershell
# Using .NET DateTime class to get the current UTC date and time
[System.DateTime]::UtcNow
```

**Sample output:**

```
Tuesday, September 5, 2023 1:46:02 PM
```

These commands and classes provide powerful and flexible options for working with dates and times in PowerShell, essential for many scripting and automation tasks.
