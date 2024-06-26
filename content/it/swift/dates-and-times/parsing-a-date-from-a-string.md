---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:46.793221-07:00
description: "Come fare: La libreria standard di Swift, Foundation, fornisce `DateFormatter`\
  \ per convertire le stringhe in oggetti `Date` e viceversa. Per analizzare\u2026"
lastmod: '2024-03-13T22:44:43.779287-06:00'
model: gpt-4-0125-preview
summary: La libreria standard di Swift, Foundation, fornisce `DateFormatter` per convertire
  le stringhe in oggetti `Date` e viceversa.
title: Analisi di una data da una stringa
weight: 30
---

## Come fare:


### Utilizzando `DateFormatter` di Foundation
La libreria standard di Swift, Foundation, fornisce `DateFormatter` per convertire le stringhe in oggetti `Date` e viceversa. Per analizzare una data da una stringa, si specifica il formato della data che corrisponde alla stringa, dopodiché si utilizza il formattatore per analizzarla.

```swift
import Foundation

let dateString = "2023-04-30"
let formatter = DateFormatter()
formatter.dateFormat = "yyyy-MM-dd"
if let date = formatter.date(from: dateString) {
    print("Data analizzata: \(date)")
} else {
    print("Analisi della data fallita")
}
// Output di esempio: Data analizzata: 2023-04-29 22:00:00 +0000
```

Nota che l'output può variare in base al tuo fuso orario.

### Utilizzando ISO8601DateFormatter
Per i formati di data ISO 8601, Swift fornisce un formattatore specializzato, `ISO8601DateFormatter`, che semplifica il processo di analisi.

```swift
import Foundation

let dateString = "2023-04-30T15:00:00+00:00"
let isoFormatter = ISO8601DateFormatter()
if let date = isoFormatter.date(from: dateString) {
    print("Data ISO8601 analizzata: \(date)")
} else {
    print("Analisi data ISO8601 fallita")
}
// Output di esempio: Data ISO8601 analizzata: 2023-04-30 15:00:00 +0000
```

### Usando una Libreria di Terze Parti: SwiftDate
Sebbene Swift fornisca strumenti robusti per l'analisi delle date, le librerie di terze parti come SwiftDate offrono ancora maggiore flessibilità e convenienza. Dopo aver aggiunto SwiftDate al tuo progetto, l'analisi diventa semplice come:

```swift
import SwiftDate

let dateString = "April 30, 2023"
if let date = dateString.toDate("MMMM dd, yyyy") {
    print("Data analizzata con SwiftDate: \(date)")
} else {
    print("Analisi della data con SwiftDate fallita")
}
// Output di esempio: Data analizzata con SwiftDate: 2023-04-30 00:00:00 +0000
```

SwiftDate semplifica l'analisi con linguaggio naturale e una vasta gamma di formati di data, rendendolo un'aggiunta potente al tuo toolkit di programmazione Swift.
