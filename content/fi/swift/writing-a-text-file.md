---
title:                "Swift: Tekstitiedoston kirjoittaminen"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Tervetuloa lukemaan uutta blogipostia suomenkielisille Swift-ohjelmoijille. Tässä artikkelissa puhumme siitä, miksi kirjoittaisimme teksti-tiedostoja ohjelmoimisessa ja miten se tapahtuu. Toivottavasti löydät tästä postauksesta hyödyllistä tietoa ja vinkkejä omiin ohjelmointiprojekteihisi.

## Miten tehdä

Teksti-tiedostojen kirjoittaminen Swiftillä on melko yksinkertaista, ja siihen on useampi tapa. Yksinkertaisin tapa on käyttää "FileHandle" -luokkaa, joka mahdollistaa tiedostoon kirjoittamisen ja lukemisen.

```
let fileManager = FileManager.default
    
// Luodaan uusi teksti-tiedosto
let fileURL = fileManager.homeDirectoryForCurrentUser.appendingPathComponent("uusiTiedosto.txt")
    
do {
    // Avataan tiedosto kirjoittamista varten
    let fileHandle = try FileHandle(forWritingTo: fileURL)
    
    // Kirjoitetaan tiedostoon tekstiä
    let teksti = "Tämä on esimerkki tekstistä."
    fileHandle.write(teksti.data(using: .utf8)!)
    
    // Suljetaan tiedosto
    fileHandle.closeFile()
} catch {
    print("Virhe tiedoston kirjoittamisessa: \(error)")
}
```

Jos haluat lisätä tekstiä olemassa olevaan tiedostoon, voit käyttää "FileHandle" -luokkaa uudelleen ja lisätä "tiedostonKirjoitustila" -parametrin avulla olemassa olevaan tiedostoon kirjoittamisen.

```
let tiedostonKirjoitustila = fileHandle.seekToEndOfFile()
fileHandle.write(teksti.data(using: .utf8)!)
```

Voit myös käyttää "OutputStream" -luokkaa, joka antaa sinulle enemmän hallintaa tiedostoon kirjoittamisessa.

```
let fileManager = FileManager.default
    
// Luodaan uusi teksti-tiedosto
let fileURL = fileManager.homeDirectoryForCurrentUser.appendingPathComponent("uusiTiedosto.txt")
    
do {
    // Avataan tiedosto kirjoittamista varten
    let tiedostonRikkiolmu = try OutputStream(url: fileURL, append: false)
    
    tiedostonRikkiolmu.open()
    
    // Kirjoitetaan tiedostoon tekstiä
    let teksti = "Tämä on esimerkki tekstistä."
    let kirjoitusTuloste = teksti.data(using: .utf8) ?? Data()
    kirjoitusTuloste.withUnsafeBytes({
        tiedostonRikkiolmu.write($0, maxLength: kirjoitusTuloste.count)
    })
    
    // Suljetaan tiedosto
    tiedostonRikkiolmu.close()
} catch {
    print("Virhe tiedoston kirjoittamisessa: \(error)")
}
```

## Syvä sukellus

Teksti-tiedostojen kirjoittamisen lisäksi voit myös käyttää Swiftiä muihin tiedostoon liittyviin toimintoihin, kuten tiedoston kopiointiin, poistamiseen ja lukemiseen. Voit myös käyttää "String" -luokkaa helpottamaan tekstin muokkaamista.

On myös tärkeää huomata, että tiedostojen kirjoittamiseen liittyy aina virheiden käsittely, jotta ohjelma ei kaatuilisi, jos jotain menee vikaan.

## Katso myös

- [Swiftin virallinen opas teksti-tiedostojen käsittelyyn](https://developer.apple.com/documentation/foundation/file_management/writing_data_to_a_file)
- [Swiftin virallinen opas tiedostojen käsittelyyn](https://developer.apple.com/documentation/foundation/file_management)
- [Swiftin virallinen opas virheiden käsittelystä](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html)