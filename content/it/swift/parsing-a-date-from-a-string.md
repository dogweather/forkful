---
title:                "Analizzare una data da una stringa"
html_title:           "Fish Shell: Analizzare una data da una stringa"
simple_title:         "Analizzare una data da una stringa"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Analisi di una Data da una Stringa in Swift

## Cos'è e Perché?

L'analisi di una data da una stringa significa convertire un testo che rappresenta una data o un'ora in un oggetto data. Questa operazione è spesso necessaria perché le date sono salvate come stringhe nei database o nei file JSON.

## Come Fare:

Per analizzare una data da una stringa in Swift, utilizzeremo la classe `DateFormatter`. Ecco un esempio con commenti nel codice per aiutare a comprendere.

```Swift
import Foundation

// Creiamo un'istanza di DateFormatter
let dateFormatter = DateFormatter()

// Impostiamo il formato della data che corrisponde alla stringa
dateFormatter.dateFormat = "dd-MM-yyyy"

// Usiamo il metodo date(from:) per analizzare la data
let date = dateFormatter.date(from: "31-12-2021")

// Stampa: "Optional(2021-12-31 00:00:00 +0000)"
print(date)
```
In questo esempio, abbiamo convertito la stringa `"31-12-2021"` in un oggetto data.

## Approfondimento

### Storia
L'analisi delle date da stringhe è una pratica comune fin dall'inizio della programmazione. Tuttavia, con l'introduzione di Swift nel 2014, Apple ha reso questa pratica molto più semplice e sicura con l'introduzione della classe `DateFormatter`.

### Alternative
Un'alternativa all'uso di `DateFormatter` è utilizzare le Classi NSCalendar e NSDateComponents, benché questo sia più laborioso e meno intuitivo.

```Swift
import Foundation

let dateStr = "31-12-2021"
let dateFmt = DateFormatter()
dateFmt.dateFormat = "dd-MM-yyyy"
let date = dateFmt.date(from: dateStr)

let calendar = Calendar.current
let components = calendar.dateComponents([.day, .month, .year], from: date!)

print("\(components.day!)-\(components.month!)-\(components.year!)")
```
### Dettagli Implementativi
`DateFormatter` segue le specifiche unicode per i formati di data, ciò lo rende molto versatile. Dovresti fare attenzione però, poiché l'uso intensivo di `DateFormatter` può essere dispendioso in termini di performance.

## Vedere Anche

1. [Date and Time Programming Guide](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/DataFormatting/Articles/dfDateFormatting10_4.html#//apple_ref/doc/uid/TP40002369-SW1): Un completo tutorial su come utilizzare `DateFormatter` dall'Apple Developer Documentation.