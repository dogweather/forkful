---
title:                "Lavorare con JSON"
date:                  2024-01-19
html_title:           "Arduino: Lavorare con JSON"
simple_title:         "Lavorare con JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
Lavorare con JSON significa gestire dati strutturati come testo leggibile. Programmatori lo fanno per scambiare dati tra server e app, e per salvare configurazioni.

## How to:
```Swift
import Foundation

// Definiamo una struct che si conforma al protocollo Codable
struct Utente: Codable {
    var nome: String
    var età: Int
}

// Creiamo un'istanza di Utente
let utente = Utente(nome: "Mario", età: 30)

// Convertiamo l'istanza in JSON con JSONEncoder
if let jsonData = try? JSONEncoder().encode(utente),
   let jsonString = String(data: jsonData, encoding: .utf8) {
    print(jsonString)
}

// Output: {"nome":"Mario","età":30}

// Convertiamo JSON in un'istanza di Utente con JSONDecoder
let jsonData = "{\"nome\":\"Mario\",\"età\":30}".data(using: .utf8)!
if let decodedUtente = try? JSONDecoder().decode(Utente.self, from: jsonData) {
    print(decodedUtente)
}

// Output: Utente(nome: "Mario", età: 30)
```

## Deep Dive
JSON, acronimo di JavaScript Object Notation, è un formato nato nei primi anni 2000. Alternative includono XML e YAML, ma JSON spicca per la sua semplicità. Swift gestisce JSON attraverso `Codable`, un protocollo introdotto in Swift 4 che rende semplice codificare/decodificare i dati.

## See Also
- La documentazione ufficiale di Swift su `Codable`: [Swift.org - Codable](https://swift.org/documentation/api-design-guidelines/#codable)
- Una guida alla gestione degli errori con JSON in Swift: [raywenderlich.com - Swift JSON](https://www.raywenderlich.com/3418439-encoding-and-decoding-in-swift)
