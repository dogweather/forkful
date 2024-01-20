---
title:                "Writing tests"
html_title:           "Arduino recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Writing tests in programming means crafting scripts that check if your code runs correctly. Programmers do this to catch bugs early, ensure reliability, and prevent future changes from breaking existing features.

## How to:
Here's a quick test for a function that adds numbers using Pester, PowerShell's testing framework. You'd typically save this script as `Add.Tests.ps1`.

```PowerShell
# Sample function to test
function Add ($a, $b) {
    return $a + $b
}

# Import Pester module
Import-Module Pester

# Define the test
Describe "Add-Function" {
    It "adds two numbers" {
        # Arrange
        $num1 = 10
        $num2 = 20
        $expected = 30

        # Act
        $result = Add $num1 $num2

        # Assert
        $result | Should -Be $expected
    }
}

# Run the test
Invoke-Pester
```

After running the script, you'd see output like:

```
Describing Add-Function
    [+] adds two numbers 75ms
Tests completed in 75ms
Tests Passed: 1, Failed: 0, Skipped: 0 NotRun: 0
```

## Deep Dive:
Historically, testing in PowerShell was much more manual before Pester was introduced. It changed the game by providing a powerful yet simple syntax for automated testing, borrowing concepts from testing frameworks in other languages. Alternatives to Pester include PSUnit and PSTest, but Pester is the most widely used and integrated directly into PowerShell Core for cross-platform support. Detailed implementation of tests involves a cycle typically called "Red, Green, Refactor", where tests are written to fail first (Red), then code is written to pass tests (Green), followed by a cleanup phase without changing behavior (Refactor).

## See Also:
- Pester's GitHub Repository: [https://github.com/pester/Pester](https://github.com/pester/Pester)