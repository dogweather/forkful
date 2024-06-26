---
date: 2024-02-03 19:03:32.163002-07:00
description: "How to: PowerShell does not have a built-in testing framework, but Pester,\
  \ a popular third-party module, is widely used for writing and running tests.\u2026"
lastmod: '2024-03-13T22:45:00.287251-06:00'
model: gpt-4-0125-preview
summary: PowerShell does not have a built-in testing framework, but Pester, a popular
  third-party module, is widely used for writing and running tests.
title: Writing tests
weight: 36
---

## How to:
PowerShell does not have a built-in testing framework, but Pester, a popular third-party module, is widely used for writing and running tests. Here's how to get started with Pester for testing your PowerShell functions.

First, install Pester if you haven't already:

```powershell
Install-Module -Name Pester -Scope CurrentUser -Force
```

Next, consider you have a simple PowerShell function you want to test, saved as `MyFunction.ps1`:

```powershell
function Get-MultipliedNumber {
    param (
        [int]$Number,
        [int]$Multiplier = 2
    )

    return $Number * $Multiplier
}
```

To test this function with Pester, create a test script named `MyFunction.Tests.ps1`. In this script, use Pester's `Describe` and `It` blocks to define the test cases:

```powershell
# Import the function to test
. .\MyFunction.ps1

Describe "Get-MultipliedNumber tests" {
    It "Multiplies number by 2 when no multiplier is provided" {
        $result = Get-MultipliedNumber -Number 3
        $result | Should -Be 6
    }

    It "Correctly multiplies number by given multiplier" {
        $result = Get-MultipliedNumber -Number 3 -Multiplier 3
        $result | Should -Be 9
    }
}
```

To run the tests, open PowerShell, navigate to the directory containing your test script, and use the `Invoke-Pester` command:

```powershell
Invoke-Pester .\MyFunction.Tests.ps1
```

Sample output will look like this, indicating whether your tests passed or failed:

```
Starting discovery in 1 files.
Discovery finished in 152ms.
[+] C:\path\to\MyFunction.Tests.ps1 204ms (182ms|16ms)
Tests completed in 204ms
Tests Passed: 2, Failed: 0, Skipped: 0 NotRun: 0
```

This output shows that both tests have passed, giving you confidence that your `Get-MultipliedNumber` function behaves as expected under the scenarios you've tested.
