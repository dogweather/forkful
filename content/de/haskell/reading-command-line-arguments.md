---
title:                "Lesen von Befehlszeilenargumenten"
html_title:           "Haskell: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Das Lesen von Befehlszeilenargumenten in Haskell ist eine nützliche Fähigkeit für Entwickler, die Programme schreiben und ausführen möchten, die mit Benutzereingaben interagieren. Durch das Verstehen dieser Funktion können Sie z.B. verschiedene Einstellungen für Ihr Programm festlegen oder es an individuelle Bedürfnisse anpassen.

## Wie geht's

Um Befehlszeilenargumente in Haskell auszulesen, müssen Sie zunächst das `System.Environment`-Modul importieren. Dann können Sie die Funktion `getArgs` verwenden, um eine Liste mit den übergebenen Argumenten zu erhalten. Diese Liste können Sie dann entsprechend verarbeiten. Hier ist ein Beispielcode:

```Haskell
import System.Environment

main = do
  args <- getArgs
  putStrLn ("Es wurden " ++ show (length args) ++ " Argumente übergeben:")
  mapM_ putStrLn args
```

Wenn wir diesen Code mit dem Befehl `runhaskell args.hs argument1 argument2` ausführen, erhalten wir die Ausgabe:

```
Es wurden 2 Argumente übergeben:
argument1
argument2
```

## Tiefer Einblick

Die Funktion `getArgs` liefert eine Liste vom Typ `IO [String]`, da es sich um eine Aktion in der `IO` Monad handelt. Dies bedeutet, dass wir innerhalb einer `do`-Blockstruktur arbeiten müssen, um das Ergebnis der `getArgs`-Funktion zu verwenden. Die Funktion `mapM_` in unserem Beispiel nimmt eine Funktion und eine Liste und führt die Funktion für jedes Element der Liste aus. Darüber hinaus können wir auch das erste Argument (den Befehlsnamen) aus der Liste der Argumente entfernen, indem wir `getProgName` verwenden.

Weitere Informationen zu Command Line Arguments in Haskell finden Sie in der offiziellen Dokumentation des `System.Environment`-Moduls [hier](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-Environment.html).

## Siehe auch

* [Einleitung zu Haskell](https://www.haskell.org/) - Offizielle Website von Haskell
* [Hoogle](https://hoogle.haskell.org/) - Eine Suchmaschine für Haskell-Funktionen und -Module
* [Learn You a Haskell](http://learnyouahaskell.com/) - Ein interaktives Online-Buch, das die Grundlagen von Haskell vermittelt