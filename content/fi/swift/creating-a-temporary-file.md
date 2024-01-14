---
title:    "Swift: Tilapäistiedoston luominen"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi luoda väliaikainen tiedosto

Väliaikaiset tiedostot ovat tärkeitä ohjelmoinnissa monilla eri tavoilla. Ne voivat auttaa tallentamaan väliaikaisia tietoja tai väliaikaisia tiedostoja, jotka ovat tarpeen tietyn tehtävän suorittamiseksi. Ne ovat myös käteviä tietojen siirtämiseen eri osioiden välillä ja voivat auttaa optimoimaan suorituskykyä. Joten jos haluat oppia lisää siitä, miten luoda väliaikaisia tiedostoja Swiftillä, jatka lukemista!

## Näin luot väliaikaisen tiedoston

```Swift
// Luodaan väliaikainen tiedoston nimi
let temporaryFileName = "temporaryFile.txt"

// Haetaan dokumenttipolku
guard let documentPath = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first else {
  fatalError("Dokumenttipolkua ei löytynyt")
}

// Yhdistetään tiedostonimi document-polkuun
let temporaryFilePath = documentPath.appendingPathComponent(temporaryFileName)

// Kirjoitetaan tiedoston sisältö
let content = "Tämä on väliaikainen tiedosto!"
do {
  try content.write(to: temporaryFilePath, atomically: false, encoding: .utf8)
} catch {
  print("Tiedoston luominen epäonnistui: ", error.localizedDescription)
}

// Tulostetaan tiedoston sisältö
print("Tiedoston sisältö: ")
do {
  let retrievedContent = try String(contentsOf: temporaryFilePath, encoding: .utf8)
  print(retrievedContent)
} catch {
  print("Tiedoston lukeminen epäonnistui: ", error.localizedDescription)
}
```

**Tulostettu sisältö:**
```
Tämä on väliaikainen tiedosto!
```

## Syventymistä väliaikaisiin tiedostoihin

Väliaikaiset tiedostot luodaan yleensä, kun tarvitaan väliaikaista tallennustilaa tiedon siirtämistä tai käsittelyä varten. Ne ovat yleensä tallennettuna sovelluksen kohdepisteen "/tmp" kansioon. Väliaikaiset tiedostot poistetaan yleensä automaattisesti käytön jälkeen. Voit myös käyttää `NSFileHandle`-luokkaa tiedoston avulla suorittamaan tietoja ja lukemaan tiedostoja. Lisäksi voit määrittää tiedoston luontipolitiikan antamalla `URL`-osoitteen `FileManager`-luokalle, esimerkiksi pitämällä tiedosto kohdistettuna vain sovelluksen elinaikana ja poistamaan sen poistumisen jälkeen.

## Katso myös

- [Swiftin virallinen dokumentaatio väliaikaisten tiedostojen luomisesta](https://developer.apple.com/documentation/foundation/filemanager/1407720-tempporarydirectory)
- [NSFileHandle-dokumentaatio Swiftissä](https://developer.apple.com/documentation/foundation/nsfilehandle)
- [Väliaikaisten tiedostojen käyttö Swiftillä](https://learnappmaking.com/temporary-files-swift-playground-dimpiaal-ad-da-f10-10-2017/)