---
title:                "Lavorare con i file csv."
html_title:           "Swift: Lavorare con i file csv."
simple_title:         "Lavorare con i file csv."
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore Swift, probabilmente avrai già familiarità con i dati strutturati e la loro rappresentazione in formato CSV. Ma se sei nuovo in questo mondo, potresti chiederti perché dovresti usare il formato CSV nelle tue applicazioni. In realtà, ci sono molte buone ragioni per farlo.

## Come fare

In Swift, ci sono diverse librerie disponibili per leggere e scrivere file CSV. Una delle più popolari è la libreria "SwiftCSV", che rende facile manipolare i dati CSV. Ecco un semplice esempio di come leggere un file CSV e stampare il suo contenuto:

```Swift
import SwiftCSV

let csvPath = Bundle.main.path(forResource: "dati", ofType: "csv") // Rileva il percorso del file CSV
let csvFile = try! CSV(url: csvPath!) // Crea un oggetto CSV con il percorso del file
let rows = csvFile.namedRows // Estrae le righe del file CSV

for row in rows { // Cicla attraverso le righe
    print(row["Nome"], row["Cognome"], row["Età"]) // Stampa i valori delle colonne di ogni riga
}
```

L'output di questo codice sarà simile a questo:

```
John Smith, 35
Sarah Anderson, 28
Henry Johnson, 41
```

## Approfondimento

Lavorare con CSV può sembrare semplice, ma ci sono alcune cose da tenere a mente se si desidera utilizzare correttamente questo formato di dati. Ad esempio, dovresti prestare attenzione alla gestione delle virgolette e dei caratteri speciali nei tuoi dati, poiché possono influire sulla struttura del file CSV.

Inoltre, è importante sapere come manipolare i dati CSV usando librerie come "SwiftCSV" o come strumento di base come il framework "Foundation". Ci sono diverse funzioni disponibili per filtrare, ordinare e manipolare i dati CSV, quindi assicurati di esplorare le opzioni e trova quella che meglio si adatta alle tue esigenze.

## Vedi anche

* [Documentazione di SwiftCSV](https://github.com/swiftcsv/SwiftCSV)
* [Documentazione di Foundation](https://developer.apple.com/documentation/foundation)
* [Tutorial su come elaborare i dati CSV in Swift](https://www.raywenderlich.com/925-swift-coding-challenge-process-csv-files)