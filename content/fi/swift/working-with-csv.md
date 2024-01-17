---
title:                "Työskentely csv:n kanssa"
html_title:           "Swift: Työskentely csv:n kanssa"
simple_title:         "Työskentely csv:n kanssa"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

CSV eli "comma-separated values" on yksinkertainen ja suosittu tapa tallentaa ja jakaa tietoja taulukkomuodossa. Se koostuu riveistä ja sarakkeista, jotka on erotettu pilkulla. Ohjelmoijat käyttävät CSV:tä usein tietojen lukuun ja kirjoittamiseen, koska se on helppo ja yleisesti tuettu tiedostomuoto.

## Miten:

```Swift
// Luodaan CSV-muodossa oleva taulukko
let data = [["1", "Apple", "10"], ["2", "Orange", "6"], ["3", "Banana", "3"]]

// Tallennetaan taulukko CSV-muodossa tiedostoon nimeltä "fruits.csv"
let csv = data.map { $0.joined(separator: ",") }.joined(separator: "\n")
try csv.write(toFile: "fruits.csv", atomically: true, encoding: .utf8)

// Luetaan CSV-tiedosto ja tulostetaan sen sisältö
if let csvString = try? String(contentsOfFile: "fruits.csv", encoding: .utf8) {
    csvString.enumerateLines { line, _ in
        let columns = line.components(separatedBy: ",")
        let id = columns[0]
        let name = columns[1]
        let quantity = columns[2]
        print("\(name): \(quantity) in stock")
    }
}
```

Tulostus:
```
Apple: 10 in stock
Orange: 6 in stock
Banana: 3 in stock
```

## Syvempi sukellus:

CSV:n esiaste oli yksinkertaisempi käyttöliittymälijstämuoto, joka kehitettiin IBM:n toimesta 1972. CSV:n suosio kasvoi 1980-luvulla, kun Microsoft käytti sitä ensimmäisessä Excel-versiossaan. Nykyään CSV on yleisesti käytössä tietojen siirrossa eri ohjelmien välillä, kuten tietokantojen ja taulukkolaskentaohjelmien välillä.

On myös muita taulukkoformaatteja, kuten JSON, jotka voivat olla parempia monimutkaisempien tietorakenteiden tallentamiseen. Mutta jos kyseessä on yksinkertainen tietojen tallentaminen ja jakaminen, niin CSV on helppo ja tehokas vaihtoehto.

CSV-tiedoston lukeminen ja kirjoittaminen voidaan toteuttaa myös itse käyttämällä esimerkiksi tekstikäsittelyohjelmaa. Tämä voi kuitenkin olla työlästä ja altis virheille, joten Swift-ohjelmointikielen avulla CSV:n käsittely on nopeampaa ja luotettavampaa.

## Katso myös:

- [Apple Swift blogi: Working with CSV Files](https://developer.apple.com/swift/blog/?id=37)
- [CSV-lukijan ja kirjoittajan kirjasto Swiftille](https://github.com/swiftcsv/SwiftCSV)
- [CSV-formaatin määritelmä](https://tools.ietf.org/html/rfc4180)