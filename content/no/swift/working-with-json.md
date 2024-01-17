---
title:                "Å jobbe med json"
html_title:           "Swift: Å jobbe med json"
simple_title:         "Å jobbe med json"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/working-with-json.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Arbeid med JSON er en vanlig oppgave for Swift-programmerere. JSON står for JavaScript Object Notation, og er et format for lagring og overføring av data. Programmerere bruker JSON for å enkelt kunne lagre og utveksle data mellom ulike applikasjoner og systemer.

## Slik gjør du det:
Her er et enkelt eksempel på hvordan du kan arbeide med JSON i Swift:

```Swift
// Opprett en dictionary med informasjon om en person
let person = ["navn": "Anna", "alder": 30, "hobbyer": ["sykling", "løping"]]

// Konverter dictionaryen til JSON-data
let jsonData = try JSONSerialization.data(withJSONObject: person)

// Skriv ut JSON-data som en lesbar tekststreng
if let jsonString = String(data: jsonData, encoding: .utf8) {
  print(jsonString)
}

// Output:
// {"navn":"Anna", "alder":30, "hobbyer":["sykling","løping"]}
```

## Dypdykk:
Historisk sett har JSON blitt brukt som et alternativ til XML, et annet format for lagring og overføring av data. JSON er mye lettere å lese og skrive for mennesker, og tar også mindre plass i datalagringen. 

I Swift er JSON-støtte innebygd i språket, ved hjelp av `JSONSerialization`-klassen. Denne klassen gjør det enkelt å konvertere data til og fra JSON-format.

For mer informasjon om arbeid med JSON i Swift, kan du sjekke ut Apple's offisielle dokumentasjon: https://developer.apple.com/documentation/foundation/jsonserialization

## Se også:
- En grundig guide til JSON i Swift: https://www.raywenderlich.com/8274-json-tutorial-for-ios-getting-started
- En sammenligning mellom JSON og XML: https://medium.com/@waqas_malik/json-vs-xml-which-one-is-better-based-on-technical-strengths-8587a834aeb0
- Informasjon om Swift's `JSONEncoder` og `JSONDecoder`-klasser for enda enklere håndtering av JSON-data.