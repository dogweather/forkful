---
date: 2024-01-26 04:45:48.712610-07:00
description: "Kuinka: Swift ei sis\xE4ll\xE4 valmista tukea kompleksiluvuille, mutta\
  \ voimme luoda oman."
lastmod: '2024-03-13T22:44:56.901291-06:00'
model: gpt-4-0125-preview
summary: "Swift ei sis\xE4ll\xE4 valmista tukea kompleksiluvuille, mutta voimme luoda\
  \ oman."
title: "Kompleksilukujen k\xE4sittely"
weight: 14
---

## Kuinka:
Swift ei sisällä valmista tukea kompleksiluvuille, mutta voimme luoda oman:

```Swift
struct ComplexNumber {
    var real: Double
    var imaginary: Double
    
    func add(_ other: ComplexNumber) -> ComplexNumber {
        return ComplexNumber(real: real + other.real, imaginary: imaginary + other.imaginary)
    }
    
    // Lisämetodeja, kuten vähennys, kertolasku, jne.
}

let first = ComplexNumber(real: 2, imaginary: 3)
let second = ComplexNumber(real: 1, imaginary: 4)
let result = first.add(second)
print("Tulos: \(result.real) + \(result.imaginary)i")
// Esimerkkitulostus: Tulos: 3.0 + 7.0i
```

## Syväsukellus
Kompleksiluvut tulivat esiin 16. vuosisadalla algebraisissa yhtälöissä. Ne ovat tärkeitä kvanttimekaniikassa, säätöteoriassa ja monilla muilla aloilla. Applen Swift ei sisällä vakiovarastokirjastoa kompleksiluvuille, toisin kuin kielet kuten Python tai C++. Vaihtoehtoja oman toteutuksen tekemiselle sisältävät Numerics-paketin, joka sisältää tuen kompleksiluvuille, tai C++ kompleksilukukirjaston kääriminen Swiftin yhteentoimivuuden avulla.

## Katso myös
- Swift Numerics: [https://github.com/apple/swift-numerics](https://github.com/apple/swift-numerics)
