---
title:    "Swift: Tiedoston kirjoittaminen"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Miksi

Kirjoittaa tekstitiedosto on tärkeä osa ohjelmointia, erityisesti kun käsitellään suuria määriä tietoa tai halutaan tallentaa tietoja pitkäaikaiseen säilytykseen.

## Kuinka tehdä

Voit luoda uuden tekstitiedoston käyttämällä Swiftin `FileManager` luokkaa. Alla olevassa koodilohkossa näet esimerkin, jossa luodaan uusi tekstitiedosto nimeltä "uusi_tiedosto.txt" ja luetaan siihen tallennettu teksti.

```Swift
let tiedostonimi = "uusi_tiedosto.txt"
let tekstinpätkä = "Tämä on uusi tiedosto!"
do {
    let tiedostonpolku = try FileManager.default.url(for: .documentDirectory,
                                                    in: .userDomainMask,
                                                    appropriateFor: nil,
                                                    create: false)
            .appendingPathComponent(tiedostonimi)
    
    try tekstinpätkä.write(to: tiedostonpolku, atomically: true,
                           encoding: String.Encoding.utf8)
    
    let ladattutiedosto = try String(contentsOf: tiedostonpolku,
                                    encoding: String.Encoding.utf8)
    print(ladattutiedosto)
} catch {
    print("Tiedoston luominen epäonnistui: \(error)")
}
```

Tämä koodilohko luo uuden tiedoston ja lukee siihen tallennetun tekstin. Voit muokata koodia tarpeen mukaan ja tallentaa haluamasi tiedot tekstitiedostoon.

## Syvällinen sukellus

Tekstitiedostot ovat yksi tärkeä osa Swiftin FileManagerin toimintoja. Voit käyttää myös muita metodeja, kuten `fileExists(atPath: )`, `copyItem(at: to: )` ja `moveItem(at: to: )` jne. Näiden avulla voit hallita tiedostoja ja kansioita ohjelmassasi.

## Katso myös

- [Swiftin dokumentaatio FileManagerin käytöstä](https://developer.apple.com/documentation/foundation/filemanager)
- [Swiftille iOS:ään -tutoriaali tekstitiedoston luomisesta](https://www.raywenderlich.com/110458/nsfilemanager-swift-tutorial)
- [Kuinka lukea ja kirjoittaa tekstitiedostoja Swiftissä -opas](https://learnappmaking.com/reading-writing-files-swift-how-to/)