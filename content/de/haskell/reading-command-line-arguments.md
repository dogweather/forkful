---
title:                "Befehlszeilenargumente lesen"
html_title:           "Arduino: Befehlszeilenargumente lesen"
simple_title:         "Befehlszeilenargumente lesen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Haskell Programmierung: Kommandozeilen-Argumente lesen

## Was und Warum?
Kommandozeilenargumente sind Parameter, die einem Programm während seiner Initialisierung übergeben werden. Sie sind entscheidend, um flexiblere und wiederverwendbare Programme zu erstellen.

## So geht's:
In Haskell verwenden wir die `getArgs` Funktion im `System.Environment` Modul, um Kommandozeilenargumente zu lesen. Hier ein Beispiel:

```Haskell
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    putStrLn ("Hallo, " ++ head args)
```

Wenn wir das Programm mit `runghc Main.hs Haskell` starten, bekommen wir:

```Shell
Hallo, Haskell
```

## Tiefgehenden Einblick
Historisch gesehen stammen Kommandozeilenargumente aus der Zeit vor grafischen Interfaces, sie sind immer noch relevant für die Erstellung effizienter und wiederverwendbarer Programme.

Alternativen zur Verwendung von `getArgs` könnten Umgebungsvariablen oder Ein-/Ausgabe für und von Dateien sein. Jede Methode hat ihre eigenen Anwendungsgebiete. Beispielsweise sind Umgebungsvariablen hilfreich, wenn mehrere Prozesse auf die gleichen Daten zugreifen müssen.

Die Implementierung von `getArgs` ist recht geradlinig. `getArgs` gibt eine Liste von Strings zurück, die die Kommandozeilenargumente sind.

## Siehe auch
Weitere Informationen finden Sie auf den folgenden Seiten:

- [Haskell Wiki: Command line argument handling](https://wiki.haskell.org/Tutorials/Programming_Haskell/Argument_handling)