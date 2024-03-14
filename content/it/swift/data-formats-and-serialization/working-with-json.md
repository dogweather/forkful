---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:37.460158-07:00
description: "Lavorare con JSON in Swift significa occuparsi di un formato di dati\
  \ leggero per lo scambio di dati. I programmatori usano JSON per trasmettere dati\
  \ tra\u2026"
lastmod: '2024-03-13T22:44:43.791275-06:00'
model: gpt-4-0125-preview
summary: "Lavorare con JSON in Swift significa occuparsi di un formato di dati leggero\
  \ per lo scambio di dati. I programmatori usano JSON per trasmettere dati tra\u2026"
title: Lavorare con JSON
---

{{< edit_this_page >}}

## Cosa e Perché?

Lavorare con JSON in Swift significa occuparsi di un formato di dati leggero per lo scambio di dati. I programmatori usano JSON per trasmettere dati tra un server e un'applicazione web perché è leggibile e facile da analizzare sia per gli esseri umani che per le macchine.

## Come fare:

Swift rende l'analisi di JSON diretta con il protocollo `Codable`. Ecco come decodificare JSON in un oggetto Swift:

```Swift
import Foundation

// Definisci un modello che si conforma a Codable
struct User: Codable {
    var name: String
    var age: Int
}

// Stringa JSON
let jsonString = """
{
    "name": "John Doe",
    "age": 30
}
"""

// Converti la stringa JSON in Data
if let jsonData = jsonString.data(using: .utf8) {
    // Decodifica i dati JSON in un oggetto User
    do {
        let user = try JSONDecoder().decode(User.self, from: jsonData)
        print("Nome: \(user.name), Età: \(user.age)")
    } catch {
        print("Errore nella decodifica del JSON: \(error)")
    }
}
```

Output di esempio:
```
Nome: John Doe, Età: 30
```

## Approfondimento

JSON (JavaScript Object Notation) è stato ampiamente adottato sin dai primi anni 2000, dopo che Douglas Crockford lo ha specificato. Ha sostituito XML in molti casi d'uso grazie alla sua sintassi più semplice e migliori prestazioni. Mentre `Codable` di Swift è il punto di riferimento per JSON, esistono alternative come `JSONSerialization` per quando si ha a che fare con tipi non conformi a Codable. Sotto il cofano, `Codable` astrae l'elaborazione di basso livello e rende la serializzazione/deserializzazione senza soluzione di continuità.

## Vedi Anche

- Esplora di più su JSON e Swift nel blog ufficiale di Swift: [Swift.org](https://swift.org/blog/)
- Controlla la documentazione di `Codable`: [Swift Codable](https://developer.apple.com/documentation/swift/codable)
- Per strutture JSON complesse, considera l'uso di librerie di terze parti come SwiftyJSON disponibile su [GitHub](https://github.com/SwiftyJSON/SwiftyJSON).
