---
title:                "Att arbeta med komplexa tal"
aliases:
- /sv/swift/working-with-complex-numbers/
date:                  2024-01-26T04:45:43.380668-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att arbeta med komplexa tal"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

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
