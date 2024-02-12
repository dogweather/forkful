---
title:                "Arbeide med CSV"
date:                  2024-02-03T19:21:37.378610-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeide med CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å jobbe med CSV-filer (kommaseparerte verdier) innebærer parsing og generering av strukturerte data fra tekstfiler der hver linje representerer en post og hver post består av felt adskilt med komma. Programmerere engasjerer seg ofte i denne aktiviteten for å enkelt importere, eksportere og manipulere tabelldata ved bruk av et format som er bredt støttet på tvers av forskjellige plattformer og programmeringsspråk, på grunn av dets enkelhet og menneskelesbare format.

## Hvordan:

I Swift er det ingen innebygd støtte for direkte parsing av CSV-filer, men du kan håndtere CSV-data ved å bruke `String`-metoder for å splitte innholdet, eller ved å dra fordel av tredjepartsbiblioteker som SwiftCSV for en mer strømlinjeformet tilnærming. Her er begge metodene:

### Manuel parsing uten eksterne biblioteker
```swift
// Vurder en enkel CSV-streng
let csvString = """
navn,alder,by
John Doe,29,New York
Jane Smith,34,Los Angeles
"""

// Splitt CSV-strengen inn i linjer
let rows = csvString.components(separatedBy: "\n")

// Trekk ut nøklene fra den første raden
let keys = rows.first?.components(separatedBy: ",")

// Iterer over radene startende fra den andre
var resultat: [[String: String]] = []
for rad in rows.dropFirst() {
    let verdier = rad.components(separatedBy: ",")
    let dict = Dictionary(uniqueKeysWithValues: zip(keys!, verdier))
    resultat.append(dict)
}

// Eksempel på utskrift
print(resultat)
// Utganger: [{"city": "New York", "age": "29", "name": "John Doe"}, {"city": "Los Angeles", "age": "34", "name": "Jane Smith"}]
```
Denne tilnærmingen er grei, men mangler robusthet, spesifikt med CSV-filer som inneholder spesialtilfeller som komma i verdier, linjeskift innenfor felt osv.

### Ved bruk av SwiftCSV-biblioteket
Først, legg til SwiftCSV i prosjektet ditt ved å inkludere det i `Package.swift`-avhengighetene dine:
```swift
.package(url: "https://github.com/swiftcsv/SwiftCSV.git", from: "0.5.6")
```
Deretter, importer og bruk det som følger:
```swift
import SwiftCSV

// Anta `csvString` er definert som ovenfor

// Opprett et CSV-objekt
if let csv = try? CSV(string: csvString) {
    // Tilgang rader som ordbøker
    let rows = csv.namedRows
    
    // Eksempel på utskrift
    print(rows)
    // Utganger: [{"city": "New York", "age": "29", "name": "John Doe"}, {"city": "Los Angeles", "age": "34", "name": "Jane Smith"}]
}
```
SwiftCSV forenkler parsing ved automatisk å håndtere nyanser som innkapslede kommaer, linjeskift i felt, og tegnkoding. Husk imidlertid å håndtere mulige feil i virkelige applikasjoner, spesielt når man håndterer eksterne datakilder.
