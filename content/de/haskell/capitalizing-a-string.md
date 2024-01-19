---
title:                "Einen String großschreiben"
html_title:           "Haskell: Einen String großschreiben"
simple_title:         "Einen String großschreiben"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# "Haskell: Zeichenketten kapitalisieren"

## Was & Warum?

Eine Zeichenkette zu kapitalisieren bedeutet, jeden Buchstaben in Großbuchstaben zu verwandeln. Programmierer machen das oft, um programminternen Text konsistent oder für die Benutzer leicht lesbar zu machen.

## Wie geht's:

Die Funktion `toUpper` aus dem `Data.Char`-Modul wird verwendet, um eine Zeichenkette zu kapitalisieren. Hier ein einfaches Beispiel:

```Haskell
import Data.Char (toUpper)

capitalizeStr :: String -> String
capitalizeStr = map toUpper
```

```shell
> capitalizeStr "hallo Welt"
"Hallo Welt"
```

So einfach ist das!

## Tief eintauchen

Die `toUpper`-Funktion in Haskell gibt es schon seit Jahren. Sie stützt sich auf die Unicode-Zeichentabelle, um die korrespondierenden Großbuchstaben zu finden.

Es könnte auch Alternativen geben, wie das Schreiben einer eigenen Funktion, die speziell auf bestimmte Anwendungsfälle zugeschnitten ist, aber generell ist `toUpper` die einfachste und effizienteste Methode.

Beachten Sie, dass `map toUpper` nur grundlegende Unicode-Zeichen korrekt behandelt. Bei komplexeren Zeichen kann es zu unerwarteten Ergebnissen kommen. Wenn Sie spezifische Anforderungen an die Behandlung von Groß- und Kleinschreibung in verschiedenen Kulturen haben, können Sie Bibliotheken wie `text-icu` in Erwägung ziehen.

## Siehe auch

* Haskell-Dokumentation für das Data.Char-Modul: http://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Char.html
* Haskell's String Typ: http://learnyouahaskell.com/starting-out#an-intro-to-lists