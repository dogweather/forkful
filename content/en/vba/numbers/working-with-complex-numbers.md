---
title:                "Working with complex numbers"
aliases: - /en/vba/working-with-complex-numbers.md
date:                  2024-02-01T21:30:18.778843-07:00
model:                 gpt-4-0125-preview
simple_title:         "Working with complex numbers"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/vba/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Working with complex numbers involves performing mathematical operations on numbers that have both a real part and an imaginary part. Programmers often engage with complex numbers in domains such as engineering, physics, and anywhere that involves solving equations that are not possible with just real numbers.

## How to:

In Visual Basic for Applications (VBA), handling complex numbers can be somewhat less straightforward compared to languages with native support for them. However, you can manage complex operations by creating functions or using existing library functions. Let's explore a basic example of addition, subtraction, multiplication, and division of complex numbers:

```vb
' Function to add complex numbers
Function AddComplex(x As String, y As String) As String
    Dim real1 As Double, imag1 As Double
    Dim real2 As Double, imag2 As Double
    
    ' Extracting real and imaginary parts from the complex numbers
    real1 = Val(Split(x, "+")(0))
    imag1 = Val(Split(x, "+")(1))
    real2 = Val(Split(y, "+")(0))
    imag2 = Val(Split(y, "+")(1))
    
    ' Performing addition
    AddComplex = (real1 + real2) & "+" & (imag1 + imag2) & "i"
End Function

' Example usage
Sub ExampleUsage()
    Dim result As String
    result = AddComplex("3+2i", "1+7i")
    Debug.Print "Result of Addition: " & result  ' Output: Result of Addition: 4+9i
End Sub
```

While this demonstrates addition, similar approaches can be adapted for subtraction, multiplication, and division. For complex operations beyond basic arithmetic, it may be worth exploring external libraries or integrating other solutions that support complex number operations more natively.

## Deep Dive:

VBA does not include built-in support for complex numbers, an aspect where it lags behind languages like Python, which has a complex number class (`complex`) or C++ with its Standard Template Library (`std::complex`). Historically, the need to manipulate complex numbers directly in VBA is relatively rare, as it is often used for automation, manipulating Office applications, and tasks that traditionally don't require complex mathematical calculations. When VBA was conceived and developed, its use cases were mainly focused on business applications rather than scientific computing, which could explain the omission.

For tasks that require extensive complex number manipulations, programmers might find using a more mathematically oriented language beneficial. However, for those committed to or restricted by the use of VBA, writing custom functions (as illustrated) or integrating with software that has these capabilities (such as MATLAB or Excel itself to some extent) are viable paths forward. Despite its limitations, creative solutions and external integrations can extend VBA's utility into domains it wasn't originally designed for, including working with complex numbers.
