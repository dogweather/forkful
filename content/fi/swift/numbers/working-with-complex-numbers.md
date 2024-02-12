---
title:                "Kompleksilukujen käsittely"
aliases: - /fi/swift/working-with-complex-numbers.md
date:                  2024-01-26T04:45:48.712610-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kompleksilukujen käsittely"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Kompleksiluvuilla on sekä reaaliosa että imaginaariosa (kuten 3 + 4i). Ohjelmoijat käyttävät niitä Swiftissä tehtäviin, kuten signaalinkäsittely, tiettyjen matemaattisten ongelmien ratkaiseminen ja fysiikan simuloiminen.

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
