---
title:                "Swift: Lavorare con i file csv"
simple_title:         "Lavorare con i file csv"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## Perché
Lavorare con CSV (Comma Separated Values) è un aspetto importante nel mondo della programmazione, soprattutto quando si tratta di analisi dei dati. Con una conoscenza di base dei CSV, è possibile manipolare, analizzare e visualizzare i dati in modo più efficace.

## Come fare
Per iniziare a lavorare con CSV in Swift, è necessario importare il framework `Foundation` e utilizzare la funzione `String(contentsOfFile:encoding)` per leggere il file CSV in una stringa. Quindi, è possibile suddividere la stringa in righe e colonne utilizzando il metodo `components(separatedBy:)`. Di seguito un esempio di codice:

```Swift
import Foundation

if let csvString = try? String(contentsOfFile: filePath, encoding: .utf8) {
    let rows = csvString.components(separatedBy: "\n")
    
    for row in rows {
        let columns = row.components(separatedBy: ",")
        print(columns)
    }
}
```

L'output di questo codice sarà una serie di array, ognuno dei quali rappresenta una riga del file CSV.

## Approfondimenti
Oltre alla semplice lettura di un file CSV, è possibile svolgere molte altre operazioni utili. Ad esempio, è possibile utilizzare il framework `CSVImporter` per gestire file CSV con righe vuote o righe contenenti virgole all'interno delle colonne. Inoltre, è possibile utilizzare la libreria `SwiftyCSV` per eseguire operazioni di filtro, ordinamento e aggregazione sui dati CSV. Ci sono molte altre librerie disponibili, quindi esplora e trova quella più adatta alle tue esigenze.

## Vedi anche
- [Importare file CSV in Swift](https://www.hackingwithswift.com/example-code/system/how-to-read-a-csv-file-into-an-array)
- [Filtrare un file CSV in Swift](https://www.avanderlee.com/swift/csv-data-import-swift/)
- [Elaborare grandi quantità di dati CSV in Swift](https://nshipster.com/swift-shelley/)