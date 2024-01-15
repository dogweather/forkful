---
title:                "Arbeiten mit json"
html_title:           "Haskell: Arbeiten mit json"
simple_title:         "Arbeiten mit json"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich mit JSON auseinandersetzen? Nun, diese Datenformat hat eine hohe Beliebtheit erreicht, vor allem als Austauschformat für Daten in Webanwendungen. Es ist auch sehr leicht lesbar und kann von den meisten Programmiersprachen verarbeitet werden, einschließlich Haskell.

## Wie Geht's

Die Verarbeitung von JSON in Haskell ist dank des `aeson`-Pakets sehr einfach. Hier ist ein Beispiel, das eine JSON-Datei liest und in ein Haskell-Datenformat konvertiert:

```Haskell
import Data.Aeson
import Data.ByteString.Lazy


main :: IO ()
main = do
  fileContents <- Data.ByteString.Lazy.readFile "data.json"
  let maybeParsedData = decode fileContents :: Maybe [Int]
  case maybeParsedData of
    Just intList -> print intList
    Nothing -> putStrLn "Fehler beim Parsen"
```

Um dieses Beispiel auszuführen, müssen Sie das `aeson`-Paket installieren, entweder mit `cabal install aeson` oder `stack install aeson`. Sie müssen auch die passende Datei `data.json` mit einer Liste von Zahlen erstellen, z.B.:

```JSON
[1, 2, 3, 4, 5]
```

Das oben gezeigte Beispiel gibt `[1, 2, 3, 4, 5]` aus, was bedeutet, dass die JSON-Daten erfolgreich in eine Liste von Integern umgewandelt wurden.

## Tiefentauchen

Das `aeson`-Paket bietet auch zusätzliche Funktionen, um JSON-Daten zu bearbeiten, z.B.:

- `encode`: Konvertiert Haskell-Daten in JSON
- `(.=)`: Ein Operator, der einem Schlüssel-Wert-Paar einen Namen zuweist
- `object`: Erzeugt ein JSON-Objekt mit einer Liste von Schlüssel-Wert-Paaren

Es ist auch möglich, komplexe Datentypen wie Records und Typsynonyme zu verwenden, um die Struktur der JSON-Daten besser abzubilden. Das `aeson`-Paket ist gut dokumentiert und bietet viele weitere Funktionen für die Arbeit mit JSON in Haskell.

Weitere Informationen zu JSON in Haskell können im [offiziellen Dokumentations-Wiki von Haskell](https://wiki.haskell.org/JSON) und im [aeson GitHub-Repository](https://github.com/bos/aeson) gefunden werden.

## Siehe Auch

- [Offizielles Haskell-Dokumentations-Wiki zu JSON](https://wiki.haskell.org/JSON)
- [aeson GitHub-Repository](https://github.com/bos/aeson)
- [Hackage-Dokumentation für das aeson-Paket](https://hackage.haskell.org/package/aeson)