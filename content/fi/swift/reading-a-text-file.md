---
title:                "Tekstitiedoston lukeminen"
html_title:           "Lua: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Tekstifilin lukeminen on tiedoston sisältämien datojen lukemista. Ohjelmoijat tekevät niin, koska tiedot säilytetään usein tekstifileissä tiedonvälitys- ja säilytystarkoituksiin.

## Kuinka se tehdään:
Seuraavassa esimerkkikoodissa näytämme, kuinka käyttää Swiftiä (versio 5.3) lukeaksesi tekstitiedoston.

```Swift
import Foundation

let fileUrl = URL(fileURLWithPath: "polku/tekstitiedostoosi.txt")

do {
    let tiedostosisalto = try String(contentsOf: fileUrl, encoding: .utf8)
    print(tiedostosisalto)
} catch {
    print("Virhe tiedoston lukemisessa: \(error)")
}
```

Tämä koodi tulostaa tekstitiedoston sisällön tai tuottaa virheilmoituksen, jos tiedostoa ei voida lukea.

## Deep Dive
Historiallisesti tiedostojen lukemisen käyttämiä metodeja on päivitetty ajan myötä paremman suorituskyvyn ja tulosten tarkkuuden saavuttamiseksi. Swift 5.3 on tuonut mukanaan parannuksia, kuten esimerkiksi `contentsOf`-metodin, joka helpottaa tiedostojen lukemista.

Vaihtoehtoisesti, voit lukea tiedostoja tavu tavulta tai käyttää `FileHandle`-objektia, jos tarvitset enemmän kontrollia.

Kuitenkin, lukiessasi tekstitiedostoja, ymmärrä, että tiedostonkäsittely voi johtaa virheisiin. Kuten edellisessä esimerkissämme, käytämme Do-Catch -looppia virheenkäsittelyyn.

## Katso myös
Tässä muutamia resursseja, jotka auttavat sinua oppimaan enemmän Swiftin tiedostonlukemisesta:

1. Swift Dokumentaatio: [https://developer.apple.com/documentation/swift](https://developer.apple.com/documentation/swift)
2. Tiedostonkäsittely Swiftissä: [https://www.hackingwithswift.com/read/0/17/reading-and-writing-strings](https://www.hackingwithswift.com/read/0/17/reading-and-writing-strings)
3. Virhekäsittely Swiftissä: [https://www.hackingwithswift.com/sixty/7/4/throwing-functions](https://www.hackingwithswift.com/sixty/7/4/throwing-functions)