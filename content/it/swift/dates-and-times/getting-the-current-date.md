---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:00.270305-07:00
description: "Ottenere la data corrente in Swift comporta l'uso della classe `Date`\
  \ per accedere alla data e all'ora in cui l'app viene eseguita. I programmatori\
  \ hanno\u2026"
lastmod: '2024-03-13T22:44:43.780344-06:00'
model: gpt-4-0125-preview
summary: Ottenere la data corrente in Swift comporta l'uso della classe `Date` per
  accedere alla data e all'ora in cui l'app viene eseguita.
title: Ottenere la data corrente
weight: 29
---

## Cosa e perché?
Ottenere la data corrente in Swift comporta l'uso della classe `Date` per accedere alla data e all'ora in cui l'app viene eseguita. I programmatori hanno bisogno di recuperare la data corrente per una miriade di motivi che vanno dalla timbratura degli eventi, all'esecuzione di calcoli sulla data, fino alla visualizzazione di date e orari in un'interfaccia utente.

## Come fare:
Il framework `Foundation` di Swift fornisce la classe `Date`, rendendo semplice ottenere la data e l'ora correnti. Ecco un esempio basilare di come ottenere la data corrente:

```swift
import Foundation

let currentDate = Date()
print(currentDate)
```

Questo produrrà un output simile a:

```
2023-04-12 07:46:23 +0000
```

Il formato dell'output segue lo standard ISO 8601, utilizzando il fuso orario UTC. Tuttavia, potresti voler formattare questa data a scopo di visualizzazione. La classe `DateFormatter` di Swift viene in soccorso:

```swift
let formatter = DateFormatter()
formatter.dateStyle = .long
formatter.timeStyle = .medium
let formattedDate = formatter.string(from: currentDate)
print(formattedDate)
```

Un esempio di output potrebbe essere:

```
12 aprile 2023 alle 10:46:23
```

Nota che il formato dell'output varierà a seconda della localizzazione del dispositivo che esegue il codice.

Per progetti che richiedono manipolazioni della data più complesse, molti sviluppatori Swift si rivolgono a librerie di terze parti come `SwiftDate`. Ecco come potresti usare `SwiftDate` per ottenere la data corrente in un fuso orario e formato specifici:

Prima, aggiungi `SwiftDate` al tuo progetto usando SPM, CocoaPods o Carthage. Poi:

```swift
import SwiftDate

let rome = Region(calendar: .gregorian, zone: .europeRome, locale: .current)
let currentDateInRome = DateInRegion(Date(), region: rome)
print(currentDateInRome.toFormat("yyyy-MM-dd HH:mm:ss"))
```

Questo potrebbe produrre:

```
2023-04-12 09:46:23
```

Utilizzando `SwiftDate`, puoi facilmente manipolare date e orari per diversi fusi orari e localizzazioni, semplificando compiti di gestione della data complessi nelle tue applicazioni Swift.
