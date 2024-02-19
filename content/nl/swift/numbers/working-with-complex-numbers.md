---
aliases:
- /nl/swift/working-with-complex-numbers/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:18.925679-07:00
description: "Complexe getallen hebben een re\xEBel deel en een imaginair deel (zoals\
  \ 3 + 4i). Programmeurs gebruiken ze in Swift voor taken zoals signaalverwerking,\
  \ het\u2026"
lastmod: 2024-02-18 23:09:02.222507
model: gpt-4-0125-preview
summary: "Complexe getallen hebben een re\xEBel deel en een imaginair deel (zoals\
  \ 3 + 4i). Programmeurs gebruiken ze in Swift voor taken zoals signaalverwerking,\
  \ het\u2026"
title: Werken met complexe getallen
---

{{< edit_this_page >}}

## Wat & Waarom?
Complexe getallen hebben een reëel deel en een imaginair deel (zoals 3 + 4i). Programmeurs gebruiken ze in Swift voor taken zoals signaalverwerking, het oplossen van bepaalde wiskundige problemen en het simuleren van natuurkunde.

## Hoe:
Swift heeft geen ingebouwde ondersteuning voor complexe getallen, maar we kunnen onze eigen maken:

```Swift
struct ComplexNumber {
    var real: Double
    var imaginary: Double
    
    func add(_ other: ComplexNumber) -> ComplexNumber {
        return ComplexNumber(real: real + other.real, imaginary: imaginary + other.imaginary)
    }
    
    // Aanvullende methoden zoals aftrekken, vermenigvuldigen, enz.
}

let first = ComplexNumber(real: 2, imaginary: 3)
let second = ComplexNumber(real: 1, imaginary: 4)
let result = first.add(second)
print("Resultaat: \(result.real) + \(result.imaginary)i")
// Voorbeelduitvoer: Resultaat: 3.0 + 7.0i
```

## Diepere duik
Complexe getallen doken op in de 16e eeuw in algebraïsche vergelijkingen. Ze zijn essentieel in kwantummechanica, regeltheorie en vele andere velden. Apple's Swift heeft, in tegenstelling tot talen zoals Python of C++, geen standaardbibliotheek voor complexe getallen. Alternatieven voor het zelf maken zijn onder meer het gebruik van het Numerics-pakket dat ondersteuning biedt voor complexe getallen of het omwikkelen van de C++ complexe bibliotheek met Swift's interoperabiliteit.

## Zie ook
- Swift Numerics: [https://github.com/apple/swift-numerics](https://github.com/apple/swift-numerics)
