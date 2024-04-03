---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:05.648349-07:00
description: "Kuinka: Swiftiss\xE4 ei ole natiivia tukea CSV-tiedostojen suoraan j\xE4\
  sent\xE4miseen, mutta voit k\xE4sitell\xE4 CSV-dataa k\xE4ytt\xE4m\xE4ll\xE4 `String`-metodeja\
  \ sis\xE4ll\xF6n\u2026"
lastmod: '2024-03-13T22:44:56.929511-06:00'
model: gpt-4-0125-preview
summary: "Swiftiss\xE4 ei ole natiivia tukea CSV-tiedostojen suoraan j\xE4sent\xE4\
  miseen, mutta voit k\xE4sitell\xE4 CSV-dataa k\xE4ytt\xE4m\xE4ll\xE4 `String`-metodeja\
  \ sis\xE4ll\xF6n jakamiseen, tai hy\xF6dynt\xE4m\xE4ll\xE4 kolmansien osapuolien\
  \ kirjastoja, kuten SwiftCSV, saadaksesi sujuvamman l\xE4hestymistavan."
title: "Ty\xF6skentely CSV:n kanssa"
weight: 37
---

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
