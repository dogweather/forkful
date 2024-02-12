---
title:                "Schreiben auf Standardfehler"
aliases:
- /de/haskell/writing-to-standard-error.md
date:                  2024-02-03T19:33:16.677103-07:00
model:                 gpt-4-0125-preview
simple_title:         "Schreiben auf Standardfehler"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?
Das Schreiben auf Standard Error (stderr) in Haskell ermöglicht es Programmen, ihre Ausgabe zwischen normalen Ergebnissen und Fehlermeldungen zu differenzieren. Dies ist entscheidend für die Signalisierung von Problemen und die Fehlersuche, ohne den Standardausgang (stdout) zu überladen, der oft die primären Daten oder das Ergebnis des Programms trägt.

## Wie geht das:
In Haskell ist das Schreiben auf stderr unkompliziert mit dem `System.IO` Modul der Basisbibliothek möglich. Unten ist ein einfaches Beispiel zur Demonstration:

```haskell
import System.IO

main :: IO ()
main = do
  hPutStrLn stderr "Dies ist eine Fehlermeldung."
```

Die Ausgabe dieses Programms auf stderr wäre:

```
Dies ist eine Fehlermeldung.
```

Wenn Sie an einer komplexeren Anwendung arbeiten oder eine bessere Kontrolle über das Logging (einschließlich Fehler) benötigen, könnten Sie sich für eine Drittanbieterbibliothek entscheiden. Eine beliebte Wahl ist `monad-logger`, das sich in den `mtl` Stil der Haskell-Programmierung integriert. Hier ist ein kleiner Ausschnitt unter Verwendung von `monad-logger`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.Logger

main :: IO ()
main = runStderrLoggingT $ do
  logErrorN "Dies ist eine Fehlermeldung unter Verwendung von monad-logger."
```

Beim Ausführen gibt die Version von `monad-logger` ähnlich eine Fehlermeldung aus, aber sie ist mit mehr Kontext wie Zeitstempeln oder Protokollebenen ausgestattet, abhängig von der Konfiguration:

```
[Error] Dies ist eine Fehlermeldung unter Verwendung von monad-logger.
```

Beide Methoden dienen dem Zweck, auf stderr zu schreiben, wobei die Wahl weitgehend von der Komplexität und den Anforderungen Ihrer Anwendung abhängt.
