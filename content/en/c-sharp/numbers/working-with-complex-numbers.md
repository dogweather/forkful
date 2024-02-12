---
title:                "Working with complex numbers"
aliases:
- /en/c-sharp/working-with-complex-numbers.md
date:                  2024-01-25T03:00:29.786885-07:00
model:                 gpt-4-1106-preview
simple_title:         "Working with complex numbers"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Complex numbers expand our number system to include imaginary numbers, allowing us to solve equations that don't have real solutions. Programmers work with them in fields like engineering, physics, and signal processing where these numbers are essential for modeling and problem-solving.

## How to:
C# has a built in `System.Numerics.Complex` structure for processing complex numbers. Here's a quick run-through:

```C#
using System;
using System.Numerics;

class ComplexNumberExample
{
    static void Main()
    {
        // Creating complex numbers
        Complex c1 = new Complex(4, 5); // 4 + 5i
        Complex c2 = Complex.FromPolarCoordinates(1, Math.PI / 4); // 1 * e^(iπ/4)

        // Basic operations
        Complex sum = c1 + c2;
        Complex difference = c1 - c2;
        Complex product = c1 * c2;
        Complex quotient = c1 / c2;

        // Output results
        Console.WriteLine($"Sum: {sum}");
        Console.WriteLine($"Difference: {difference}");
        Console.WriteLine($"Product: {product}");
        Console.WriteLine($"Quotient: {quotient}");
        Console.WriteLine($"Magnitude of c1: {c1.Magnitude}");
        Console.WriteLine($"Phase of c1: {c1.Phase}");
    }
}
```

And that’ll output:

```
Sum: (4.70710678118655, 5.70710678118655)
Difference: (3.29289321881345, 4.29289321881345)
Product: (-1.00000000000001, 9)
Quotient: (0.6, 0.8)
Magnitude of c1: 6.40312423743285
Phase of c1: 0.896055384571344
```

## Deep Dive
Complex numbers, consisting of a real and imaginary part (often notated as a + bi), have been around since the 17th century. Italian mathematician Gerolamo Cardano is credited with their early development. In programming, dealing with complex numbers involves understanding and managing these two distinct parts.

While C#'s `System.Numerics.Complex` is robust and integrated into the language, other languages like Python offer similar functionality with `cmath` or third-party libraries. And if you're working in an older version of C# or a .NET version that doesn't support `System.Numerics`, you might have to roll your own complex number class or find a library.

Internally, the operations on complex numbers use floating-point arithmetic which can introduce rounding errors. So, when implementing algorithms that use complex numbers extensively, it's key to remember this and consider the impact on precision and accuracy.

## See Also
1. C# Reference for `System.Numerics.Complex`: https://learn.microsoft.com/en-us/dotnet/api/system.numerics.complex
2. A deeper dive into the mathematics of complex numbers: https://mathworld.wolfram.com/ComplexNumber.html
3. For alternative implementations and libraries, check out Math.NET Numerics: https://numerics.mathdotnet.com/
