---
title:                "Writing tests"
date:                  2024-02-01T13:31:52.123123-07:00
model:                 gpt-4-0125-preview
simple_title:         "Writing tests"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/vba/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?

Writing tests in Visual Basic for Applications (VBA) is about creating snippets of code that automatically check if your other pieces of code are doing exactly what you expect them to do. Programmers do it to catch bugs early, ensure code quality, and safeguard against future changes breaking functionality.

## How to:

Unfortunately, VBA doesn't have a built-in testing framework like some other programming environments. But don't worry; we can improvise with a bit of creativity. To illustrate, let's say we have a simple function that adds two numbers:

```Visual Basic for Applications
Function AddNumbers(a As Integer, b As Integer) As Integer
    AddNumbers = a + b
End Function
```

To test this, we can create a subroutine that checks if the output of `AddNumbers` is what we expect it to be. Here's a rudimentary test:

```Visual Basic for Applications
Sub TestAddNumbers()
    Dim expectedResult As Integer
    Dim result As Integer
    
    expectedResult = 5
    result = AddNumbers(2, 3)
    
    If result = expectedResult Then
        Debug.Print "Test Passed"
    Else
        Debug.Print "Test Failed: Expected " & expectedResult & ", but got " & result
    End If
End Sub
```

When you run `TestAddNumbers`, you should see "Test Passed" in the Immediate Window (press Ctrl+G in the VBA editor to view it). This simple test checks if our `AddNumbers` function correctly adds 2 and 3 to make 5. If we change the inputs or the `AddNumbers` function, and something goes wrong, this test will let us know by failing.

## Deep Dive

Testing in VBA isn't as straightforward or integrated as in more modern programming languages. Languages like Python, for instance, have robust frameworks like pytest that make writing tests a breeze. However, even without a fancy framework, writing manual tests in VBA is still a worthwhile practice. It enables you to verify your code's functionality and can save you from introducing or overlooking bugs.

Historically, VBA hasn't seen the development of widespread external testing frameworks, partially because VBA is often used for quick, in-the-moment solutions in Office applications, rather than for building software where testing is a critical part of the development process. Despite this, the principle of testing your code is universal and applying even a simple testing method in VBA can improve the reliability and maintainability of your macros and functions.

When more complex testing is required, some developers turn to writing their tests in another language or using external tools to simulate user interactions for UI-based testing. However, for most practical VBA uses, a simple custom testing routine like the one demonstrated above should suffice to ensure your code is doing what you intended.
