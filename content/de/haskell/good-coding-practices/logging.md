---
date: 2024-01-26 01:07:04.543913-07:00
description: "Protokollierung in der Programmierung bedeutet im Grunde, eine Spur\
  \ von Brotkrumen in Form von aufgezeichneten Ereignissen oder Nachrichten zu\u2026"
lastmod: '2024-03-13T22:44:53.937643-06:00'
model: gpt-4-1106-preview
summary: "Protokollierung in der Programmierung bedeutet im Grunde, eine Spur von\
  \ Brotkrumen in Form von aufgezeichneten Ereignissen oder Nachrichten zu hinterlassen,\
  \ die verwendet werden k\xF6nnen, um zu verfolgen, was Ihre Anwendung zu jedem gegebenen\
  \ Zeitpunkt tut."
title: Protokollierung
weight: 17
---

## Wie geht das:
In Haskell kann die Protokollierung unter Verwendung von Bibliotheken wie `monad-logger` oder `hslogger` implementiert werden. Hier ist ein schnelles Beispiel mit `monad-logger`:

```Haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Logger
import Control.Monad.IO.Class (liftIO)

logBeispiel :: LoggingT IO ()
logBeispiel = do
    logInfoN "Starte die Anwendung..."
    liftIO $ putStrLn "Erledige kritische Arbeit..."
    logErrorN "Ups! Etwas ist schiefgelaufen."

main :: IO ()
main = runStdoutLoggingT logBeispiel

{- Beispiel-Ausgabe
[Info] Starte die Anwendung...
Erledige kritische Arbeit...
[Fehler] Ups! Etwas ist schiefgelaufen.
-}
```

Dieses einfache Beispiel zeigt, wie Sie Protokollanweisungen in Ihren Code einstreuen können, um Einblicke zu erhalten, was zur Laufzeit passiert. `logInfoN` und `logErrorN` werden verwendet, um Informations- und Fehlermeldungen zu protokollieren.

## Tiefergehend:
Protokollierung hat sich von einfachen Druckanweisungen zu ausgeklügelten Protokollierungsframeworks entwickelt. Historisch gesehen waren Protokolle nur Textausgaben auf einer Konsole oder Datei, aber jetzt beinhalten sie strukturierte Daten, die von verschiedenen Werkzeugen analysiert und verarbeitet werden können.

In Haskell kann die Protokollierung in einem rein funktionalen Stil durchgeführt werden, der das explizite Übergeben von Protokollaktionen beinhaltet, oder unter Verwendung von monadischen Kontexten für Unreinheit, wobei Protokollierer implizit durch die Berechnung geführt werden.

Die Bibliothek `hslogger` ist beispielsweise traditioneller und veränderbarer im Vergleich zu `monad-logger`. `monad-logger` bietet eine Integration in den Monad-Stack und bietet mehr Flexibilität in Bezug auf Ausgabeformatierung und -kontrolle. Beide Bibliotheken ermöglichen es Ihnen, Protokollebenen festzulegen, die bei der Filterung von Protokollnachrichten basierend auf ihrer Wichtigkeit helfen. Protokollebenen umfassen Debug, Info, Notice, Warning, Error, Critical, Alert und Emergency.

Haskells Ansatz zur Protokollierung steht oft im Einklang mit seiner Betonung auf Typsicherheit und Reinheit. Protokolle können so gehandhabt werden, dass selbst wenn die Protokollierung fehlschlägt, dies nicht dazu führen wird, dass die Hauptanwendung aufgrund von Haskells robusten Fehlerbehandlungsfähigkeiten abstürzt.

## Siehe auch:
- [`monad-logger` Dokumentation auf Hackage](https://hackage.haskell.org/package/monad-logger)
- [`hslogger` Paket auf Hackage](https://hackage.haskell.org/package/hslogger)
- [Real World Haskell, Kapitel 19, über Fehlerbehandlung](http://book.realworldhaskell.org/read/error-handling.html)
- [The Logging Facade for Haskell (log-base)](https://hackage.haskell.org/package/log-base)
