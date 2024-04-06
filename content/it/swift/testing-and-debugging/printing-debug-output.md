---
date: 2024-01-20 17:53:30.308462-07:00
description: "How to: (Come fare:) Prima di Swift c'era Objective-C, ma non offre\
  \ un comando diretto come `print()`. Si usava `NSLog()`, pi\xF9 verboso e con informazioni\u2026"
lastmod: '2024-04-05T22:50:57.564804-06:00'
model: gpt-4-1106-preview
summary: (Come fare:) Prima di Swift c'era Objective-C, ma non offre un comando diretto
  come `print()`.
title: Stampa dell'output di debug
weight: 33
---

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
