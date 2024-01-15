---
title:                "Työskentelytäuölle csv:n kanssa."
html_title:           "Swift: Työskentelytäuölle csv:n kanssa."
simple_title:         "Työskentelytäuölle csv:n kanssa."
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## Miksi

CSV-tiedostot (Comma-Separated Values) ovat yleisiä tiedostotyyppejä, joita käytetään tiedon tallentamiseen ja jakamiseen erityisesti taulukkomuodossa. Nämä tiedostot ovat suosittuja erityisesti liiketoiminta- ja tieteellisissä sovelluksissa, ja siksi on hyödyllistä tietää, kuinka työskennellä niiden kanssa ohjelmoinnin kautta.

## Kuinka

CSV-tiedostojen käsittely Swift-ohjelmoinnin avulla on melko helppoa. Tämä esimerkki näyttää kuinka lukea CSV-tiedosto, jossa on sarakkeita "nimi" ja "ikä", ja tulostaa sen sisältö konsoliin:

```Swift
if let csvPath = Bundle.main.path(forResource: "henkilötiedot", ofType: "csv") {
    do {
        let csvData = try String(contentsOfFile: csvPath, encoding: .utf8)
        let csvLines = csvData.components(separatedBy: "\n")
        
        for line in csvLines {
            let person = line.components(separatedBy: ",")
            let name = person[0]
            let age = person[1]
            print("\(name) on \(age) vuotta vanha.")
        }
    } catch {
        print("Virhe luettaessa tiedostoa.")
    }
} else {
    print("Tiedostoa ei löytynyt.")
}
```

Tulostus:

```
Matti on 32 vuotta vanha.
Maria on 28 vuotta vanha.
Timo on 45 vuotta vanha.
```

Huomaa, että osa CSV-tiedostoista voi sisältää rivinvaihtoja, jotka on otettava huomioon koodissa. Tässä esimerkissä käytämme ```components(separatedBy: "\n")``` -metodia jakamaan tiedoston rivit taulukoksi.

## Syväsukellus

CSV-tiedostojen käsittelyyn on olemassa monia erilaisia vaihtoehtoja Swiftissä. Voit esimerkiksi käyttää valmiita kirjastoja, kuten [SwiftCSV](https://github.com/swiftcsv/SwiftCSV), joka tarjoaa monia hyödyllisiä toimintoja tiedostojen lukemiseen ja kirjoittamiseen. Voit myös käyttää [Swiftin sisäänrakennettua](https://developer.apple.com/documentation/foundation/nscsvparser) ```CSVParser``` -luokkaa, jonka avulla voit käsitellä CSV-tiedostoja asynkronisesti.

On myös tärkeää huomata, että CSV-tiedostot voivat sisältää monia erilaisia muotoiluja ja näiden muotoilujen käsittelyyn on erilaisia tekniikoita. Jos haluat syvällisempää tietoa CSV-tiedostojen käsittelystä, tämä [artikkeli](https://csv-solutions.org/getting-started/) tarjoaa kattavan oppaan erilaisiin käsittelytapoihin.

## Katso myös

- [SwiftCSV](https://github.com/swiftcsv/SwiftCSV)
- [Foundation Framework: CSVParser](https://developer.apple.com/documentation/foundation/nscsvparser)
- [CSV Solutions](https://csv-solutions.org/getting-started/)