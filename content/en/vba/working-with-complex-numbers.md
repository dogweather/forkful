---
title:                "Working with complex numbers"
date:                  2024-02-01T13:31:58.938728-07:00
model:                 gpt-4-0125-preview
simple_title:         "Working with complex numbers"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/vba/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Complex numbers, those quirky yet fundamental figures with a real part and an imaginary part (think `3 + 4i`), are essential in various computations, especially in engineering. Programmers learn to manipulate them to solve problems in fields like signal processing or electrical engineering simulations.

## How to:

Visual Basic for Applications (VBA) doesn't have a built-in type for complex numbers, but that won't stop us. We'll roll up our sleeves and define our own. Here's a quick guide on how to handle complex numbers in VBA with some coding muscle.

First, define a Type for complex numbers:

```basic
Type Complex
    realPart As Double
    imaginaryPart As Double
End Type
```

Now, let's add functions to perform operations, starting with addition:

```basic
Function AddComplex(c1 As Complex, c2 As Complex) As Complex
    Dim result As Complex
    result.realPart = c1.realPart + c2.realPart
    result.imaginaryPart = c1.imaginaryPart + c2.imaginaryPart
    AddComplex = result
End Function
```

How about multiplication? It's a bit trickier but totally manageable:

```basic
Function MultiplyComplex(c1 As Complex, c2 As Complex) As Complex
    Dim result As Complex
    result.realPart = (c1.realPart * c2.realPart) - (c1.imaginaryPart * c2.imaginaryPart)
    result.imaginaryPart = (c1.imaginaryPart * c2.realPart) + (c1.realPart * c2.imaginaryPart)
    MultiplyComplex = result
End Function
```

Let's see these functions in action:

```basic
Sub DemoComplexOperations()
    Dim num1 As Complex
    Dim num2 As Complex
    Dim result As Complex

    ' Initialize complex numbers
    num1.realPart = 3
    num1.imaginaryPart = 4
    num2.realPart = 1
    num2.imaginaryPart = -2

    'Addition
    result = AddComplex(num1, num2)
    Debug.Print "Addition: ", result.realPart; " + "; result.imaginaryPart; "i"

    'Multiplication
    result = MultiplyComplex(num1, num2)
    Debug.Print "Multiplication: ", result.realPart; " + "; result.imaginaryPart; "i"
End Sub
```

Sample output for our little demo:

```
Addition: 4 + 2i
Multiplication: 11 + -5i
```

## Deep Dive

Historically, VBA wasn't designed with advanced mathematical operations like complex number arithmetic in mind. It evolved from earlier versions of BASIC with a focus on automating tasks within the Microsoft Office suite rather than crunching complex numbers. This is why, unlike languages such as Python, which boast dedicated libraries and types for complex arithmetic (thanks to its `cmath` module), in VBA, we improvise by defining our own types and functions.

That being said, this manual approach in VBA, though a bit cumbersome, shows the language's versatility. For calculations that demand extensive complex number arithmetic, one might consider integrating VBA with more mathematically inclined languages or software. MATLAB, for instance, provides a rich set of functionalities for dealing with complex numbers and may be more efficient for heavy computational tasks. However, when working within the confines of the Microsoft Office ecosystem, crafting your own VBA solutions, as we've demonstrated, is perfectly viable and often quite rewarding.
