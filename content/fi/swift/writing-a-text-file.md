---
title:                "Swift: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Tekstitiedoston kirjoittaminen on tärkeä osa Swift-ohjelmointia, sillä se mahdollistaa tietojen tallentamisen ja jakamisen eri sovellusten ja käyttäjien välillä. Se myös auttaa organisoimaan ja hallitsemaan tietoja selkeästi ja helposti.

## Näin teet sen

Käyttöliittymäkomponentit ovat tärkeä osa tekstitiedoston kirjoittamista. Voit esimerkiksi luoda tekstikentän, johon käyttäjä syöttää haluamansa tiedot. Tämän jälkeen voit tallentaa tiedot tekstifileeseen seuraavanlaisella koodilla:

```Swift
let text = "Terve, maailma!"
let filename = "tervetuloa.txt"

do {
    let documentsDirectoryURL = try FileManager.default.url(for: .documentDirectory, in: .userDomainMask, appropriateFor: nil, create: false)
    let fileURL = documentsDirectoryURL.appendingPathComponent(filename).appendingPathExtension("txt")
    print("Tiedosto tallennettu osoitteeseen: \(fileURL)")

    try text.write(to: fileURL, atomically: true, encoding: .utf8)
} catch {
    print("Tiedoston tallennus ei onnistunut: \(error)")
}
```

Kun suoritat tämän koodin, teksti tallentuu "tervetuloa.txt" -nimiseen tiedostoon dokumenttikansioon. Voit myös lukea ja muokata tekstifileitä käyttämällä vastaavia metodeja.

## Syväsukellus

Tekstitiedostojen muuttaminen on myös helppoa Swiftissä. Voit esimerkiksi lisätä uuden rivin tekstifileeseen seuraavalla tavalla:

```Swift
let newLine = "\nUusi rivi!"
let fileURL = documentsDirectoryURL.appendingPathComponent(filename).appendingPathExtension("txt")

// Tarkistetaan ettei tiedoston sisältö ole tyhjä
if let fileContents = try? String(contentsOf: fileURL, encoding: .utf8) {
  // Lisätään uusi rivi tiedoston sisältöön
  let updatedContent = fileContents + newLine
    
  do {
    // Tallennetaan päivitetty sisältö takaisin tiedostoon
    try updatedContent.write(to: fileURL, atomically: true, encoding: .utf8)
  } catch {
    print("Tiedoston muokkaus ei onnistunut: \(error)")
  }
}
```

Jos haluat lisätä uuden rivin tiedoston alkuun, voit käyttää metodia `write(to:atomically:encoding:)` sen sijaan, että käyttäisit `String(contentsOf:encoding:)` ja `write(to:options:)` -metodeja.

## Katso myös

* [Apple Developer Documentation - File System Basics](https://developer.apple.com/documentation/foundation/file_system_basics)
* [Swift.org - Swift Language Guide](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html)