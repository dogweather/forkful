---
title:                "Swift: Lavorare con json"
simple_title:         "Lavorare con json"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/working-with-json.md"
---

{{< edit_this_page >}}

## Perché

JSON è un formato di dati ampiamente utilizzato nel mondo della programmazione, soprattutto per lo scambio di informazioni tra client e server. Imparare a lavorare con JSON può rendere il tuo codice più efficiente e versatile.

## Come fare

Per iniziare a lavorare con JSON in Swift, è necessario importare il framework `Foundation` nella tua applicazione. Successivamente, puoi utilizzare il metodo `JSONSerialization` per convertire i dati JSON in un formato utilizzabile in Swift.

```Swift
import Foundation

// Dati JSON di esempio
let json = """
{
    "nome": "Mario",
    "cognome": "Rossi",
    "eta": 30,
    "hobby": ["cucinare", "viaggiare"]
}
""".data(using: .utf8)

do {
    // Converto i dati JSON in un oggetto Swift
    if let dict = try JSONSerialization.jsonObject(with: json!, options: []) as? [String : Any] {
        // Accesso ai dati
        let nome = dict["nome"] as? String
        let cognome = dict["cognome"] as? String
        let eta = dict["eta"] as? Int
        let hobby = dict["hobby"] as? [String]
        
        // Output
        print(nome)
        print(cognome)
        print(eta)
        print(hobby)
    }
} catch {
    print(error)
}
```

Il codice sopra mostrerà i seguenti dati in console:

```
Optional("Mario")
Optional("Rossi")
Optional(30)
Optional(["cucinare", "viaggiare"])
```

## Approfondimento

Oltre alla semplice conversione dei dati JSON, è possibile utilizzare la funzione `JSONSerialization` per recuperare dati specifici all'interno di un JSON array o oggetto. Inoltre, ci sono molti framework e librerie disponibili per semplificare ulteriormente il processo di lavorare con JSON in Swift.

## Vedi anche

- [Documentazione ufficiale Apple su JSONSerialization](https://developer.apple.com/documentation/foundation/jsonserialization)
- [SwiftyJSON - un framework popolare per lavorare con JSON in Swift](https://github.com/SwiftyJSON/SwiftyJSON)
- [Ray Wenderlich tutorial su come analizzare JSON in Swift](https://www.raywenderlich.com/7685493-reading-and-writing-json-data-in-swift-5)