---
aliases:
- /fi/swift/working-with-csv/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:05.648349-07:00
description: "CSV-tiedostoilla (Comma-Separated Values, pilkulla erotetut arvot) ty\xF6\
  skentely sis\xE4lt\xE4\xE4 rakenteellisen datan j\xE4sennyksen ja tuottamisen teksti-\u2026"
lastmod: 2024-02-18 23:09:08.014352
model: gpt-4-0125-preview
summary: "CSV-tiedostoilla (Comma-Separated Values, pilkulla erotetut arvot) ty\xF6\
  skentely sis\xE4lt\xE4\xE4 rakenteellisen datan j\xE4sennyksen ja tuottamisen teksti-\u2026"
title: "Ty\xF6skentely CSV:n kanssa"
---

{{< edit_this_page >}}

## Mikä & Miksi?

CSV-tiedostoilla (Comma-Separated Values, pilkulla erotetut arvot) työskentely sisältää rakenteellisen datan jäsennyksen ja tuottamisen teksti-tiedostoista, joissa jokainen rivi edustaa merkintää ja jokainen merkintä koostuu pilkuilla erotetuista kentistä. Ohjelmoijat osallistuvat usein tähän toimintaan, jotta he voivat helposti tuoda, viedä ja manipuloida taulukkomuotoista dataa käyttämällä formaattia, joka on laajasti tuettu eri alustoilla sekä ohjelmointikielillä, johtuen sen yksinkertaisuudesta ja ihmislukuisesta muodosta.

## Kuinka:

Swiftissä ei ole natiivia tukea CSV-tiedostojen suoraan jäsentämiseen, mutta voit käsitellä CSV-dataa käyttämällä `String`-metodeja sisällön jakamiseen, tai hyödyntämällä kolmansien osapuolien kirjastoja, kuten SwiftCSV, saadaksesi sujuvamman lähestymistavan. Tässä ovat molemmat menetelmät:

### Manuaalinen jäsentäminen ilman ulkoisia kirjastoja
```swift
// Harkitse yksinkertaista CSV-merkkijonoa
let csvString = """
name,age,city
John Doe,29,New York
Jane Smith,34,Los Angeles
"""

// Jaetaan CSV-merkkijono riveihin
let rows = csvString.components(separatedBy: "\n")

// Otetaan avaimet ensimmäiseltä riviltä
let keys = rows.first?.components(separatedBy: ",")

// Iteroidaan rivien yli alkaen toisesta
var result: [[String: String]] = []
for row in rows.dropFirst() {
    let values = row.components(separatedBy: ",")
    let dict = Dictionary(uniqueKeysWithValues: zip(keys!, values))
    result.append(dict)
}

// Näyte tulos
print(result)
// Tuloste: [{"city": "New York", "age": "29", "name": "John Doe"}, {"city": "Los Angeles", "age": "34", "name": "Jane Smith"}]
```
Tämä lähestymistapa on suoraviivainen, mutta puuttuu robustiutta, erityisesti CSV-tiedostojen kanssa, jotka sisältävät erikoistapauksia kuten pilkkuja arvoissa, rivinvaihtoja kentissä jne.

### Käyttäen SwiftCSV-kirjastoa
Lisää ensin SwiftCSV projektiisi sisällyttämällä se `Package.swift`-riippuvuuksiisi:
```swift
.package(url: "https://github.com/swiftcsv/SwiftCSV.git", from: "0.5.6")
```
Sen jälkeen, tuo ja käytä sitä seuraavasti:
```swift
import SwiftCSV

// Oletetaan, että `csvString` on määritelty yllä

// Luo CSV-objekti
if let csv = try? CSV(string: csvString) {
    // Pääsy rivit sanakirjoina
    let rows = csv.namedRows
    
    // Näyte tulos
    print(rows)
    // Tuloste: [{"city": "New York", "age": "29", "name": "John Doe"}, {"city": "Los Angeles", "age": "34", "name": "Jane Smith"}]
}
```
SwiftCSV yksinkertaistaa jäsentämistä automaattisesti käsittelemällä vivahteita kuten kapseloidut pilkut, rivinvaihdot kentissä ja merkistökoodauksen. Muista kuitenkin käsitellä mahdollisia virheitä todellisissa sovelluksissa, erityisesti kun käsitellään ulkoisia datalähteitä.
