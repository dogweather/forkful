---
title:                "Lavorare con json"
html_title:           "Swift: Lavorare con json"
simple_title:         "Lavorare con json"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/working-with-json.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Lavorare con JSON è fondamentale per gli sviluppatori che hanno a che fare con il trasferimento di dati tra diversi sistemi o piattaforme. JSON, acronimo di JavaScript Object Notation, è un formato di scrittura e lettura dei dati strutturato e leggibile dalle macchine. Per questa ragione, è ampiamente utilizzato nella comunicazione tra applicazioni web e mobile.

## Come fare:
Per lavorare con JSON in Swift, è necessario utilizzare la classe codable. In questo modo è possibile convertire automaticamente i dati tra il formato JSON e i tipi di dati Swift. Di seguito un esempio di codice per decodificare un oggetto JSON in un oggetto di tipo Person:

```Swift
struct Person: Codable {
    var name: String
    var age: Int
}

let json = """
{
    "name": "John",
    "age": 30
}
""".data(using: .utf8)!

let decodedPerson = try JSONDecoder().decode(Person.self, from: json)
```

L'output sarà un oggetto di tipo Person con il nome "John" e l'età 30.

## Approfondimento:
JSON è stato sviluppato nei primi anni 2000 come una semplice alternativa al formato XML per la trasmissione dei dati. Grazie alla sua semplicità e leggibilità, è diventato presto uno standard ampiamente utilizzato. Alcune alternative a JSON includono YAML e XML, ma JSON rimane uno dei formati più comuni per il trasferimento dei dati.

Un altro concetto importante da comprendere quando si lavora con JSON è la serializzazione, cioè il processo di trasformare una struttura dati in una rappresentazione serializzata, spesso in formato JSON, per consentirne la trasmissione su una rete o il salvataggio su disco.

## Vedi anche:
Ulteriori informazioni su Codable in Swift: https://developer.apple.com/documentation/swift/codable

Introduzione a JSON: https://www.json.org/