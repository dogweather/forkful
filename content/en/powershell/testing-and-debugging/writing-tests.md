---
title:                "Writing tests"
aliases:
- /en/powershell/writing-tests.md
date:                  2024-02-03T19:03:32.163002-07:00
model:                 gpt-4-0125-preview
simple_title:         "Writing tests"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?

Writing tests in PowerShell involves creating scripts that automatically validate the functionality of your PowerShell code, ensuring it behaves as expected. Programmers do this to catch bugs early, simplify code maintenance, and ensure that code modifications don't inadvertently break existing functionality.

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
