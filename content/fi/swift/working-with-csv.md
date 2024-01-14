---
title:                "Swift: Töitä tehdään csv:n kanssa"
simple_title:         "Töitä tehdään csv:n kanssa"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## Miksi käyttää CSV-tiedostoja Swift-ohjelmoinnissa?

CSV-tiedostot ovat yleinen tapa tallentaa taulukkomuotoista dataa. Ne ovat käteviä esimerkiksi Excelissä tai Google Sheetsissä käsiteltäväksi, mutta myös Swift-ohjelmoinnissa. Käyttämällä CSV-tiedostoja, voit helposti lukea ja kirjoittaa suuria määriä dataa ilman monimutkaisia tietokantakyselyitä.

## Miten käsitellä CSV-tiedostoja Swiftillä?

Aloita tuomalla Foundation-kirjasto, jotta voit käyttää CSV-käsittelyyn tarvittavia toimintoja.

```
import Foundation
```

Seuraavaksi voit avata CSV-tiedoston ja lukea sen sisällön käyttäen `contentsofFile`-funktiota. Voit myös käyttää `components(separatedBy: )`-funktiota jakamaan tiedoston rivit ja sarakkeet eri muuttujiin.

```
if let file = Bundle.main.path(forResource: "data", ofType: "csv") {
    do {
        let content = try String(contentsOfFile: file)
        let rows = content.components(separatedBy: "\n")
        
        for row in rows {
            let values = row.components(separatedBy: ",")
            print(values)
        }
    } catch {
        print(error)
    }
}
```

Tämän esimerkin tulostuksessa jokainen rivi on jaettu omiksi arvoikseen, joten voit käsitellä dataa helposti tarpeidesi mukaan.

## Syvempi sukellus CSV:n maailmaan

CSV-tiedostoissa on tiettyjä huomioitavia asioita, kuten se että kaikki tiedot ovat tekstimuodossa. Sinun täytyy siis muuntaa ne tarvittaessa haluamaasi muotoon Swiftin tyypiksi.

Voit myös käyttää ainutlaatuista `CSVReader`-luokkaa, joka helpottaa CSV-tiedostojen käsittelyä ja tarjoaa lisää toimintoja, kuten otsikkorivien käsitteleminen.

```
do {
    let csv = try CSVReader.init(string: content)
    while let row = csv.next() {
        let firstName = row["FirstName"]
        let lastName = row["LastName"]
        let age = Int(row["Age"] ?? "0")
        // käsittele dataa
    }
} catch {
    print(error)
}
```

## Katso myös
- [Apple:n dokumentaatio CSV-käsittelystä Swiftillä](https://developer.apple.com/documentation/foundation/csvreadingoptions)
- [CSV.swift - helppo CSV-käsittelykirjasto Swiftillä](https://github.com/yaslab/CSV.swift)
- [Kantoraketti: Puhdasta Swiftiä hyödyntävä CSV-tiedostolukija](https://kantoraketti.com/2014/01/reading-csv-file-swift/)