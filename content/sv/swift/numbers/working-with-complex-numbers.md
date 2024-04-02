---
date: 2024-01-26 04:45:43.380668-07:00
description: "Komplexa tal har en reell del och en imagin\xE4r del (som 3 + 4i). Programmerare\
  \ anv\xE4nder dem i Swift f\xF6r uppgifter som signalbehandling, att l\xF6sa vissa\u2026"
lastmod: '2024-03-13T22:44:38.245281-06:00'
model: gpt-4-0125-preview
summary: "Komplexa tal har en reell del och en imagin\xE4r del (som 3 + 4i). Programmerare\
  \ anv\xE4nder dem i Swift f\xF6r uppgifter som signalbehandling, att l\xF6sa vissa\u2026"
title: Att arbeta med komplexa tal
weight: 14
---

## Vad & Varför?
Komplexa tal har en reell del och en imaginär del (som 3 + 4i). Programmerare använder dem i Swift för uppgifter som signalbehandling, att lösa vissa matematiska problem och att simulera fysik.

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
