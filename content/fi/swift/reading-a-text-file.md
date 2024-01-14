---
title:                "Swift: Tekstitiedoston lukeminen"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Miksi lukisi tiedostoa ohjelmointitekstistä? Tämän kysymyksen tarkoituksena on valaista, miksi tämä taito voi olla hyödyllinen tai tarpeellinen ohjelmointikehityksessä.

## Kuinka tehdä se

Seuraavaksi esittelemme yksinkertaisen tavan lukea tekstitiedosto Swift-kielellä käyttäen Foundation-kirjastoa.

Ensinnäkin, tarvitset tekstitiedoston, jota haluat lukea. Voit käyttää esimerkiksi seuraavaa tekstiä:

```Swift
Tervetuloa lukemaan tekstiä Swiftille!
Tässä on esimerkki tietokoneohjelmasta.
Toivomme, että nautit lukemisesta!
```

Seuraavaksi, voit käyttää Foundation-kirjastoa avataksesi tekstitiedoston ja lukeaksesi sen sisältöä. Koodisi tulisi näyttää tältä:

```Swift
import Foundation

//Valitse tekstitiedosto käyttämällä URL-osoitetta.
let fileURL = URL(fileURLWithPath: "polku/tekstitiedosto.txt")

do {
    //Avaa tiedosto ja lue sen sisältö käyttäen String-initialisaattoria.
    let fileContent = try String(contentsOf: fileURL)
    print(fileContent) //Tulostaa tekstitiedoston sisällön konsoliin.
} catch {
    //Jos virhe tapahtuu, tulostetaan virheviesti.
    print("Virhe avatessa tekstitiedostoa: \(error)")
}
```

Kun suoritat tämän koodin, tulee tulosteena olla tekstin sisältö, joka näyttää tältä:

```
Tervetuloa lukemaan tekstiä Swiftille!
Tässä on esimerkki tietokoneohjelmasta.
Toivomme, että nautit lukemisesta!
```

## Syventävää tietoa

Tähän mennessä olemme läpikäyneet yksinkertaisen tavan lukea tekstitiedosto Swiftillä. On kuitenkin hyvä tietää, että Foundation-kirjastossa on myös muita tapoja lukea tekstitiedostoja, kuten Data- ja InputStream-luokat. Näitä luokkia voi käyttää esimerkiksi silloin, kun haluat lukea tiedoston pätkissä tai käsitellä tietoa binäärimuodossa.

Kannattaa myös pitää mielessä, että tekstitiedostot eivät aina ole yksinkertaisia ja niiden lukeminen voi joskus johtaa virheisiin. On tärkeää käsitellä mahdollisia virheitä ja poikkeuksia koodissasi, jotta ohjelma toimii luotettavasti.

## Katso myös

- [Swiftin viralliset ohjekirjat Foundation-kirjastosta](https://developer.apple.com/documentation/foundation)
- [Stack Overflow -kysymys ja vastaus tekstitiedoston lukemisesta Swiftillä](https://stackoverflow.com/questions/58494728/how-to-read-data-from-text-file-in-swift)