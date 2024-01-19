---
title:                "Einen String in Kleinbuchstaben umwandeln"
html_title:           "Elm: Einen String in Kleinbuchstaben umwandeln"
simple_title:         "Einen String in Kleinbuchstaben umwandeln"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Konvertierung eines Strings in Kleinbuchstaben in Haskell 

## Was & Warum?
Die Konvertierung eines Strings in Kleinbuchstaben ("lower case") bedeutet, alle Großbuchstaben in einem String durch ihre kleinbuchstabige Entsprechung zu ersetzen. Dies wird in der Programmierung oft durchgeführt, um eine konsistente und leicht vergleichbare Ausgabe zu gewährleisten.

## So geht's:
In Haskell ist dies eine einfache Aufgabe mit der eingebauten Funktion `map` und `toLower` aus dem `Data.Char` Modul. Die Funktion `toLower` konvertiert ein einzelnes Zeichen in Kleinbuchstaben.

```Haskell
import Data.Char(toLower)

lowercase :: String -> String
lowercase = map toLower
```

Beispiel:

```Haskell
main :: IO ()
main = do
    let result = lowercase "What a Wonderful World!"
    putStrLn result
```

Ausgabe:

```
what a wonderful world!
```
    
## Deep Dive
Ursprünglich gab es in Formalcomputer keine Funktion zum Konvertieren von Großbuchstaben in Kleinbuchstaben. Diese Funktion wurde später hinzugefügt, um verschiedene Textmanipulationen und -vergleiche in Programmen zu vereinfachen.

Es gibt mehrere Wege, eine ähnliche Funktionalität zu erreichen. Man könnte zum Beispiel eine rekursive Funktion schreiben, die über den String iteriert und jedes Zeichen einzeln in Kleinbuchstaben umwandelt. Allerdings ist die Verwendung der vordefinierten `map` and `toLower` Funktionen in Haskell der einfachste und bevorzugte Weg.

Die `toLower` Funktion verwendet Unicode Punkt-zu-Punkt-Mapping, um das Zeichen in Kleinbuchstaben umzuwandeln und kann deshalb mit allen Unicode-Buchstaben umgehen.

## Siehe auch
- [Haskell Wiki](https://wiki.haskell.org/index.php?title=Main_Page&oldid=62660)
- [Data.Char Dokumentation](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char.html)
- [Funktionale Programmierung in Haskell](https://www.futurelearn.com/courses/functional-programming-haskell)