---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:38.765589-07:00
description: "Lavorare con file CSV (Comma-Separated Values, ovvero Valori Separati\
  \ da Virgola) comporta l'analisi e la generazione di dati strutturati da file di\
  \ testo\u2026"
lastmod: '2024-03-13T22:44:43.792297-06:00'
model: gpt-4-0125-preview
summary: "Lavorare con file CSV (Comma-Separated Values, ovvero Valori Separati da\
  \ Virgola) comporta l'analisi e la generazione di dati strutturati da file di testo\
  \ dove ogni riga rappresenta un record e ogni record \xE8 costituito da campi separati\
  \ da virgole."
title: Lavorare con i CSV
weight: 37
---

## Cosa & Perché?

Lavorare con file CSV (Comma-Separated Values, ovvero Valori Separati da Virgola) comporta l'analisi e la generazione di dati strutturati da file di testo dove ogni riga rappresenta un record e ogni record è costituito da campi separati da virgole. I programmatori si impegnano spesso in questa attività per importare, esportare e manipolare facilmente dati tabellari utilizzando un formato ampiamente supportato su diverse piattaforme e linguaggi di programmazione, grazie alla sua semplicità e formato leggibile dall'uomo.

## Come fare:

In Swift, non c'è un supporto nativo per l'analisi dei file CSV direttamente, ma è possibile gestire i dati CSV utilizzando i metodi di `String` per dividere i contenuti, o sfruttando librerie di terze parti come SwiftCSV per un approccio più semplificato. Ecco entrambi i metodi:

### Analisi Manuale senza Librerie Esterne
```swift
// Considera una semplice stringa CSV
let csvString = """
name,age,city
John Doe,29,New York
Jane Smith,34,Los Angeles
"""

// Dividi la stringa CSV in righe
let rows = csvString.components(separatedBy: "\n")

// Estrai le chiavi dalla prima riga
let keys = rows.first?.components(separatedBy: ",")

// Itera sulle righe partendo dalla seconda
var result: [[String: String]] = []
for row in rows.dropFirst() {
    let valori = row.components(separatedBy: ",")
    let dict = Dictionary(uniqueKeysWithValues: zip(keys!, valori))
    result.append(dict)
}

// Output di esempio
print(result)
// Produce: [{"city": "New York", "age": "29", "name": "John Doe"}, {"city": "Los Angeles", "age": "34", "name": "Jane Smith"}]
```
Questo approccio è diretto ma manca di solidità, specialmente con file CSV che contengono casi speciali come virgole nei valori, interruzioni di linea all'interno dei campi, ecc.

### Utilizzando la Libreria SwiftCSV
Primo, aggiungi SwiftCSV al tuo progetto includendolo nelle tue dipendenze di `Package.swift`:
```swift
.package(url: "https://github.com/swiftcsv/SwiftCSV.git", from: "0.5.6")
```
Poi, importa e usalo come segue:
```swift
import SwiftCSV

// Assumi `csvString` sia definita come sopra

// Crea un oggetto CSV
if let csv = try? CSV(string: csvString) {
    // Accedi alle righe come dizionari
    let rows = csv.namedRows
    
    // Output di esempio
    print(rows)
    // Produce: [{"city": "New York", "age": "29", "name": "John Doe"}, {"city": "Los Angeles", "age": "34", "name": "Jane Smith"}]
}
```
SwiftCSV semplifica l'analisi automaticamente trattando le sfumature come le virgole incapsulate, le interruzioni di linea nei campi e la codifica dei caratteri. Tuttavia, ricorda di gestire possibili errori nelle applicazioni reali, specialmente quando si tratta di fonti di dati esterne.
