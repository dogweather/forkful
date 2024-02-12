---
title:                "Stampa dell'output di debug"
aliases:
- /it/swift/printing-debug-output/
date:                  2024-01-20T17:53:30.308462-07:00
model:                 gpt-4-1106-preview
simple_title:         "Stampa dell'output di debug"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa e Perché?)
Stampare l'output di debug aiuta a vedere cosa succede nel tuo codice. Usiamo questa tecnica per controllare i valori delle variabili e capire il flusso dell'esecuzione del programma.

## How to: (Come fare:)
```Swift
// Stampa semplice
print("Ciao, mondo!")

// Concatenazione di stringhe e variabili
let nome = "Vale"
print("Ciao, \(nome)!")

// Stampa su più linee
print("""
Questo è un testo
su più righe.
""")

// Stampa con separator e terminator
let frutti = ["mele", "arance", "banane"]
print(frutti[0], frutti[1], frutti[2], separator: " * ", terminator: " → Fine della lista.\n")
```
Risultato:
```
Ciao, mondo!
Ciao, Vale!
Questo è un testo
su più righe.
mele * arance * banane → Fine della lista.
```

## Deep Dive (Approfondimento)
Prima di Swift c'era Objective-C, ma non offre un comando diretto come `print()`. Si usava `NSLog()`, più verboso e con informazioni aggiuntive come timestamp e nome dell'app. In Swift, `print()` ha reso tutto più snello e diretto. Swift fornisce anche `debugPrint()` per una rappresentazione più dettagliata adatta al debug. In contesti di produzione, si considerano log frameworks come CocoaLumberjack o SwiftyBeaver, per funzionalità avanzate come livelli di log e output su file.

## See Also (Vedi Anche)
- Documentazione Swift sul `print()` function: https://developer.apple.com/documentation/swift/1541053-print
- Circa logging avanzato: CocoaLumberjack (https://github.com/CocoaLumberjack/CocoaLumberjack) e SwiftyBeaver (https://github.com/SwiftyBeaver/SwiftyBeaver)
