---
title:                "Lavorare con i file CSV"
date:                  2024-01-19
html_title:           "Bash: Lavorare con i file CSV"
simple_title:         "Lavorare con i file CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa & Perché?)
Lavorare con i CSV significa manipolare dati salvati in un formato testuale semplice, separati da virgole (Comma-Separated Values). I programmatori lo fanno perché i CSV sono universali, leggibili da persone e macchine, ed è banale esportarli/leggerli da una moltitudine di strumenti.

## How to: (Come fare:)
```Swift
import Foundation

// Definire una struttura per i dati
struct Persona {
    var nome: String
    var eta: Int
}

// Funzione per leggere un file CSV e trasformarlo in una lista di strutture `Persona`
func leggiCSV(daPercorso percorso: String) -> [Persona]? {
    do {
        let dati = try String(contentsOfFile: percorso, encoding: .utf8)
        let righe = dati.split(separator: "\n").map { String($0) }
        
        return righe.map { riga in
            let valori = riga.split(separator: ",")
            return Persona(nome: String(valori[0]), eta: Int(valori[1]) ?? 0)
        }
    } catch {
        print("Errore durante la lettura del file: \(error)")
        return nil
    }
}

// Uso
if let percorsoFileCSV = Bundle.main.path(forResource: "persone", ofType: "csv") {
    if let persone = leggiCSV(daPercorso: percorsoFileCSV) {
        for persona in persone {
            print("\(persona.nome): \(persona.eta) anni")
        }
    }
}
```
Output:
```
Mario: 30 anni
Giulia: 25 anni
Luca: 40 anni
```

## Deep Dive (Approfondimento)
CSV nasce negli anni '70 e rappresenta il minimo comun denominatore dei sistemi di archiviazione tabellare. Rispetto a formati come XML o JSON, i CSV sono più leggibili e meno verbosi, ma mancano di standardizzazione definita. Swift non ha librerie standard per CSV, quindi si usano soluzioni personalizzate o di terze parti. Applicazioni come Excel, Numbers e Google Sheets esportano e importano CSV, consolidandone l'utilizzo.

## See Also (Vedi anche)
- Documentazione ufficiale Swift: [https://swift.org/documentation/](https://swift.org/documentation/)
- Un articolo su come analizzare i CSV con Swift: [https://www.hackingwithswift.com/example-code/system/how-to-parse-a-csv-file-into-an-array](https://www.hackingwithswift.com/example-code/system/how-to-parse-a-csv-file-into-an-array)
