---
title:                "Lavorare con csv"
html_title:           "Swift: Lavorare con csv"
simple_title:         "Lavorare con csv"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## Cosa è & Perché?
In breve, lavorare con CSV è un modo per gestire e manipolare grandi quantità di dati tabellari. I programmatori spesso utilizzano CSV per importare e esportare dati da varie fonti, come fogli di calcolo o database.

## Come fare:
Ecco un esempio di come leggere un file CSV e stampare le righe:

```Swift
if let url = Bundle.main.url(forResource: "dati", withExtension: "csv") {
    do {
        let data = try String(contentsOf: url)
        let rows = data.components(separatedBy: "\n")
        for row in rows {
            print(row)
        }
    } catch {
        print("Errore: \(error)")
    }
}
```

Output:

1, Gatto, 10
2, Cane, 5
3, Uccello, 2

## Approfondimento:
CSV, acronimo di Comma Separated Values, è un formato di file molto utilizzato per l'importazione e l'esportazione di dati. In passato era uno standard molto popolare per le basi di dati, ma è stato ampiamente sostituito da formati più strutturati come JSON o XML. Tuttavia, CSV rimane utile per gestire grandi quantità di dati rapidamente e facilmente.

Esistono anche molti strumenti e librerie che semplificano il lavoro con CSV in Swift, come ad esempio la libreria SwiftCSV. Inoltre, molte app hanno la funzione di esportare dati in formato CSV per facilitare l'analisi dei dati da parte degli utenti.

Per implementare una lettura e scrittura di CSV più sofisticata, è possibile utilizzare le API di Foundation, come ad esempio CSVParser e CSVWriter.

## Vedi anche:
- [SwiftCSV](https://github.com/willowtreeapps/swift-csv)
- [Apple Foundation Framework](https://developer.apple.com/documentation/foundation)