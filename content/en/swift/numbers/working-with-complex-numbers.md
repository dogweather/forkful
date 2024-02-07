---
title:                "Working with complex numbers"
date:                  2024-01-25T02:59:36.400318-07:00
model:                 gpt-4-1106-preview
simple_title:         "Working with complex numbers"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Complex numbers have a real part and an imaginary part (like 3 + 4i). Programmers use them in Swift for tasks like signal processing, solving certain math problems, and simulating physics.

## How to:
Swift doesn't have built-in complex number support, but we can roll our own:

```Swift
struct ComplexNumber {
    var real: Double
    var imaginary: Double
    
    func add(_ other: ComplexNumber) -> ComplexNumber {
        return ComplexNumber(real: real + other.real, imaginary: imaginary + other.imaginary)
    }
    
    // Additional methods like subtraction, multiplication, etc.
}

let first = ComplexNumber(real: 2, imaginary: 3)
let second = ComplexNumber(real: 1, imaginary: 4)
let result = first.add(second)
print("Result: \(result.real) + \(result.imaginary)i")
// Sample Output: Result: 3.0 + 7.0i
```

## Deep Dive
Complex numbers popped up in the 16th century in algebraic equations. They're essential in quantum mechanics, control theory, and many other fields. Apple's Swift doesn't have a standard library for complex numbers, unlike languages like Python or C++. Alternatives to rolling your own include using Numerics package which includes complex numbers support or wrapping C++ complex library with Swift's interoperability.

## See Also
- Swift Numerics: [https://github.com/apple/swift-numerics](https://github.com/apple/swift-numerics)
