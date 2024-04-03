---
date: 2024-01-26 04:45:43.380668-07:00
description: "Hur man g\xF6r: Swift har inte inbyggt st\xF6d f\xF6r komplexa tal,\
  \ men vi kan skapa v\xE5rt eget."
lastmod: '2024-03-13T22:44:38.245281-06:00'
model: gpt-4-0125-preview
summary: "Swift har inte inbyggt st\xF6d f\xF6r komplexa tal, men vi kan skapa v\xE5\
  rt eget."
title: Att arbeta med komplexa tal
weight: 14
---

## Hur man gör:
Swift har inte inbyggt stöd för komplexa tal, men vi kan skapa vårt eget:

```Swift
struct ComplexNumber {
    var real: Double
    var imaginary: Double
    
    func add(_ other: ComplexNumber) -> ComplexNumber {
        return ComplexNumber(real: real + other.real, imaginary: imaginary + other.imaginary)
    }
    
    // Ytterligare metoder som subtraktion, multiplikation, etc.
}

let first = ComplexNumber(real: 2, imaginary: 3)
let second = ComplexNumber(real: 1, imaginary: 4)
let result = first.add(second)
print("Resultat: \(result.real) + \(result.imaginary)i")
// Exempel på utskrift: Resultat: 3.0 + 7.0i
```

## Fördjupning
Komplexa tal dök upp under 1500-talet i algebraiska ekvationer. De är nödvändiga inom kvantmekanik, reglerteknik och många andra fält. Apples Swift har inte ett standardbibliotek för komplexa tal, till skillnad från språk som Python eller C++. Alternativ till att skapa eget inkluderar att använda Numerics-paket som inkluderar stöd för komplexa tal eller att inkapsla C++ komplexa bibliotek med Swifts interoperabilitet.

## Se även
- Swift Numerics: [https://github.com/apple/swift-numerics](https://github.com/apple/swift-numerics)
