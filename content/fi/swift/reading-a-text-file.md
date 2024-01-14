---
title:                "Swift: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Lukemalla tekstiedostoja ohjelmoijat voivat helposti käsitellä suuria määriä tietoa, kuten tekstejä ja laskentataulukoita, ilman tarvetta syöttää niitä manuaalisesti. Tämä tekee ohjelmoinnista nopeampaa ja vähentää virheiden mahdollisuutta.

## Miten

Tekstiedostojen lukeminen Swiftissä on helppoa käyttämällä `String`-luokan `contentsOfFile`-metodia. Alla on esimerkki koodista, joka lukee tekstiedoston nimeltä "data.txt" ja tulostaa sen sisällön konsoliin:

```Swift
if let path = Bundle.main.path(forResource: "data", ofType: "txt") { // etsii tiedoston polun
    do {
         let data = try String(contentsOfFile: path) // lukee tiedoston sisällön merkkijonona
         print(data) // tulostaa sisällön konsoliin
     } catch {
         print("Virhe: \(error)") // käsittelee mahdolliset virheet
     }
}
```

Käyttämällä `components(separatedBy:)`-metodia, voit jakaa tiedoston sisällön haluamallasi tavalla. Esimerkiksi seuraava koodi jakaa tiedoston rivit ja tulostaa ne yksitellen:

```Swift
if let path = Bundle.main.path(forResource: "data", ofType: "txt") {
    do {
         let data = try String(contentsOfFile: path)
         let lines = data.components(separatedBy: "\n") // jakaa merkkijonon rivinvaihdon kohdalta
         for line in lines {
             print(line) // tulostaa rivin konsoliin
         }
     } catch {
         print("Virhe: \(error)")
     }
}
```

## Syväsukellus

Tekstiedostojen lukeminen Swiftissä perustuu `String`-luokan `contentsOfFile`-metodiin, joka luo merkkijonon tiedoston sisällöstä. Tämä metodi heittää `throws`-avainsanan avulla mahdolliset virheet. `Bundle`-luokka puolestaan auttaa löytämään tiedoston polun tietyssä hakemistossa. Tiedoston sisältöä voi sitten käsitellä `components(separatedBy:)`-metodilla, joka jakaa merkkijonon halutun erotinmerkin kohdalta.

## Katso myös

- [Swiftin virallinen dokumentaatio tekstiedostojen lukemisesta](https://developer.apple.com/documentation/foundation/string/1413584-contentsoffile)
- [Kaikki `String`-luokan saatavilla olevat metodit](https://developer.apple.com/documentation/swift/string)