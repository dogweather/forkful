---
title:    "Swift: Tarkista onko hakemisto olemassa"
keywords: ["Swift"]
---

{{< edit_this_page >}}

# Miksi tarkistaa, onko hakemisto olemassa?

On tilanteita, joissa ohjelmoijan täytyy tarkistaa, onko tietty hakemisto olemassa ennen kuin se voidaan käyttää. Tämä voi johtua esimerkiksi siitä, että halutaan tallentaa tiedostoja tiettyyn sijaintiin tai että halutaan varmistaa, ettei jo olemassa olevia tiedostoja korvata vahingossa. Tässä blogikirjoituksessa käymme läpi, miten tarkistaa hakemiston olemassaolo Swift-ohjelmoinnissa.

## Kuinka tarkistaa hakemiston olemassaolo

Tarkistamiseen käytetään FileManager-luokkaa ja sen `fileExists(atPath:)` -metodia. Metodi ottaa parametrinaan hakemiston polun ja palauttaa totuusarvon, joka kertoo, onko hakemisto olemassa vai ei.

```Swift
// Luodaan FileManager-instanssi
let fileManager = FileManager.default

// Tarkistetaan, onko "Documents" -hakemisto olemassa
if fileManager.fileExists(atPath: "Documents") {
    print("Hakemisto on olemassa.")
} else {
    print("Hakemistoa ei löytynyt.")
}

// Output:
// Hakemisto on olemassa.
```

## Syvemmälle tarkasteluun

`fileExists(atPath:)` -metodi tarkistaa vain hakemiston olemassaolon, eikä se tarkista, onko kyseinen polku varmasti hakemisto. Tämä tarkoittaa, että se voi palauttaa `true` -arvon myös silloin, kun polku johtaa tiedostoon. Jos haluat varmistaa, että kyseessä todella on hakemisto, voit käyttää metodia `isDirectory` luokasta `URLResourceValues`. 

```Swift
// Luodaan URL-instanssi hakemistolle
let url = URL(fileURLWithPath: "Documents")

// Tarkistetaan, onko kyseessä hakemisto
do {
    let resourceValues = try url.resourceValues(forKeys: [.isDirectoryKey])
    if resourceValues.isDirectory != nil {
        print("Polku johtaa hakemistoon.")
    } else {
        print("Polku johtaa tiedostoon.")
    }
} catch {
    print(error.localizedDescription)
}

// Output:
// Polku johtaa hakemistoon.
```

## Katso myös

Jos haluat tutustua tarkemmin hakemistojen käsittelyyn Swift-ohjelmoinnissa, kannattaa tutustua seuraaviin artikkeleihin:

- [Tiedostojen ja kansioiden luominen Swift-ohjelmoinnissa](https://www.appcoda.com/swift-filemanager/)
- [Swift-tiedostojen käsittely ja tallentaminen](https://medium.com/@nomanbinhussein/working-with-files-in-swift-794f40a5f4b8)
- [Swift-lukijärjestelmän tutkiminen](https://www.hackingwithswift.com/example-code/system/how-to-examine-systems-using-filemanager-and-url)

Toivottavasti tämä blogikirjoitus auttoi sinua ymmärtämään, miten tarkistaa hakemiston olemassaolo Swiftissä. Hyviä ohjelmointihetkiä!