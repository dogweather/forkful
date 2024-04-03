---
date: 2024-01-26 04:45:51.880988-07:00
description: "Jak to zrobi\u0107: Swift nie ma wbudowanego wsparcia dla liczb zespolonych,\
  \ ale mo\u017Cemy stworzy\u0107 w\u0142asne."
lastmod: '2024-03-13T22:44:35.748834-06:00'
model: gpt-4-0125-preview
summary: "Swift nie ma wbudowanego wsparcia dla liczb zespolonych, ale mo\u017Cemy\
  \ stworzy\u0107 w\u0142asne."
title: Praca z liczbami zespolonymi
weight: 14
---

## Jak to zrobić:
Swift nie ma wbudowanego wsparcia dla liczb zespolonych, ale możemy stworzyć własne:

```Swift
struct ComplexNumber {
    var real: Double
    var imaginary: Double
    
    func add(_ other: ComplexNumber) -> ComplexNumber {
        return ComplexNumber(real: real + other.real, imaginary: imaginary + other.imaginary)
    }
    
    // Dodatkowe metody takie jak odejmowanie, mnożenie itp.
}

let first = ComplexNumber(real: 2, imaginary: 3)
let second = ComplexNumber(real: 1, imaginary: 4)
let result = first.add(second)
print("Rezultat: \(result.real) + \(result.imaginary)i")
// Przykładowe wyjście: Rezultat: 3.0 + 7.0i
```

## Wnikliwe spojrzenie
Liczby zespolone pojawiły się w XVI wieku w równaniach algebraicznych. Są niezbędne w mechanice kwantowej, teorii sterowania i wielu innych dziedzinach. Swift od Apple'a nie ma standardowej biblioteki dla liczb zespolonych, w przeciwieństwie do języków takich jak Python czy C++. Alternatywy dla tworzenia własnych obejmują użycie pakietu Numerics, który zawiera wsparcie dla liczb zespolonych lub opakowanie biblioteki zespolonej C++ za pomocą interoperacyjności Swifta.

## Zobacz też
- Swift Numerics: [https://github.com/apple/swift-numerics](https://github.com/apple/swift-numerics)
