---
title:                "Praca z liczbami zespolonymi"
date:                  2024-01-26T04:45:51.880988-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z liczbami zespolonymi"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Liczby zespolone mają część rzeczywistą i część urojoną (jak 3 + 4i). Programiści używają ich w Swift do zadań takich jak przetwarzanie sygnałów, rozwiązywanie pewnych problemów matematycznych oraz symulacje fizyczne.

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
