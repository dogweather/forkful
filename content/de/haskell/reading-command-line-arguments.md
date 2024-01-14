---
title:                "Haskell: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Das Lesen von Befehlszeilenargumenten ist ein wichtiger Teil der Programmierung, da es ermöglicht, Nutzerinteraktion zu ermöglichen und Programme flexibler zu gestalten. Das Verstehen dieses Konzepts ist daher für jeden angehenden Haskell-Programmierer von großer Bedeutung.

## So geht's

Das Lesen von Befehlszeilenargumenten in Haskell ist eine einfache Aufgabe, die mit Hilfe der `System.Environment`-Bibliothek durchgeführt werden kann. 

```Haskell
import System.Environment

main = do
    args <- getArgs
    putStrLn ("Hallo, " ++ args !! 0 ++ "! Willkommen bei meinem Haskell-Blog!")
```

Die `getArgs`-Funktion gibt eine Liste von Argumenten zurück, die an das Programm übergeben wurden. In diesem Beispiel wird das erste Argument verwendet, um eine personalisierte Willkommensnachricht auszugeben. 

Die Ausgabe dieses Codes könnte wie folgt aussehen, wenn das Programm mit dem Argument "Lena" ausgeführt wird:

```
Hallo, Lena! Willkommen bei meinem Haskell-Blog!
```

## Tiefgründiger Einblick

Zusätzlich zu den Befehlszeilenargumenten kann das `System.Environment`-Modul auch verwendet werden, um auf verschiedene Umgebungsvariablen zuzugreifen. Diese könnten z. B. die aktuellen Einstellungen der Shell oder des Systems sein und können hilfreich sein, um Ihr Haskell-Programm anzupassen.

## Siehe auch

- [Haskell-Dokumentation zu Befehlszeilenargumenten](https://www.haskell.org/cabal/users-guide/developing-packages.html#accessing-command-line-arguments)
- [Haskell-Dokumentation zu Umgebungsvariablen](https://www.haskell.org/cabal/users-guide/developing-packages.html#accessing-the-environment)
- [Interaktive Übung zum Lesen von Befehlszeilenargumenten](https://dev.stephendiehl.com/hask/#command-line-arguments)