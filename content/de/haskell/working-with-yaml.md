---
title:                "Arbeiten mit YAML"
html_title:           "Haskell: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

# Warum
Ich meine, es gibt viele Gründe, in Haskell zu arbeiten, sei es wegen seiner funktionalen Programmierweise, seiner Mächtigkeit oder seiner Leichtigkeit. Aber was ist mit YAML? Nun, YAML ist eine einfache und menschenlesbare Datenformatierungssprache, die besonders nützlich ist, wenn man mit Konfigurationsdateien arbeitet. Mit Haskell können wir YAML in unsere Anwendungen integrieren und von seiner Einfachheit und Lesbarkeit profitieren.

# Wie geht das?
Um YAML in Haskell zu verwenden, müssen wir die Bibliothek "yaml" importieren. Als erstes müssen wir unser YAML-Dokument in einen Haskell-Datentyp umwandeln, indem wir die Funktion "decodeFileThrow" anwenden. Schauen wir uns ein Beispiel an:

```
import Data.Yaml

data Book = Book
    { title :: String
    , author :: String
    , year :: Int
    }

main :: IO ()
main = do
    book <- decodeFileThrow "book.yaml" :: IO (Maybe Book)
    case book of
        Nothing -> putStrLn "Fehler beim Dekodieren der YAML-Datei."
        Just b -> print b
```

Im obigen Beispiel wird das YAML-Dokument "book.yaml" in den Datentyp "Book" umgewandelt. Wenn es erfolgreich ist, wird es ausgegeben, sonst wird eine Fehlermeldung angezeigt. Beachte, dass wir den Datentyp "Maybe Book" verwenden, da das YAML-Dokument möglicherweise nicht dem vorgegebenen Schema entspricht.

# Tiefer in die Materie
Wenn wir genauer in die Bibliothek "yaml" schauen, werden wir viele weitere Funktionen finden, die uns dabei helfen, YAML in Haskell zu verwenden. Zum Beispiel gibt es die Funktion "encode", mit der wir einen Haskell-Datentyp in YAML umwandeln können. Oder die Funktion "decodeThrow", die ähnlich wie "decodeFileThrow" funktioniert, aber Daten aus einem String anstatt aus einer Datei liest. Es gibt auch Funktionen, mit denen wir mit YAML-Dateien arbeiten und diese analysieren können. Wir empfehlen, sich mit den verschiedenen Funktionen vertraut zu machen, um die Bibliothek optimal zu nutzen.

# Siehe auch
- Offizielle Dokumentation der "yaml" Bibliothek: https://hackage.haskell.org/package/yaml
- Ein Tutorial zur Verwendung von YAML in Haskell: https://www.stackbuilders.com/tutorials/haskell/yaml/
- Weitere Informationen über YAML: https://yaml.org/