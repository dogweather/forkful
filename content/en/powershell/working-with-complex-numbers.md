---
title:                "Working with complex numbers"
date:                  2024-01-25T03:00:03.460077-07:00
model:                 gpt-4-1106-preview
simple_title:         "Working with complex numbers"

category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Complex numbers, the ones with a real part and an imaginary part (like 3 + 4i), are vital in fields like engineering, physics, and data science. Programmers use them for simulations, signal processing, and solving specific types of math problems.

## How to:
PowerShell doesn't have built-in complex number support, so you either roll your own solution or use .NET's `System.Numerics.Complex`.

```PowerShell
# Let's make complex numbers using .NET
[Reflection.Assembly]::LoadWithPartialName("System.Numerics") | Out-Null

# Create complex numbers
$complex1 = [System.Numerics.Complex]::new(3, 4) # 3 + 4i
$complex2 = [System.Numerics.Complex]::new(1, 2) # 1 + 2i

# Add two complex numbers
$sum = [System.Numerics.Complex]::Add($complex1, $complex2) # 4 + 6i

# Multiply two complex numbers
$product = [System.Numerics.Complex]::Multiply($complex1, $complex2) # -5 + 10i

# Display the results
"Sum: $sum"
"Product: $product"
```
Output:
```
Sum: (4, 6)
Product: (-5, 10)
```

## Deep Dive
Complex numbers were developed in the 16th century to solve equations that didn't have solutions in the realm of real numbers. They're now a cornerstone of modern mathematics.

PowerShell's reliance on .NET for complex number support means performance is solid. Alternatives include third-party libraries or other programming languages like Python, where complex numbers are a native data type.

## See Also
- [System.Numerics.Complex Structure](https://docs.microsoft.com/en-us/dotnet/api/system.numerics.complex)
- [Complex Number Arithmetic in Python](https://docs.python.org/3/library/cmath.html)
