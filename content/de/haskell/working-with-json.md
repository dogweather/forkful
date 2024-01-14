---
title:                "Haskell: Arbeiten mit json"
simple_title:         "Arbeiten mit json"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## Warum

JSON ist eine sehr häufig verwendete Form zur Übertragung von Daten in der Programmierung, und daher ist das Verständnis von JSON in der heutigen Zeit unerlässlich. Wenn du also ein*e Programmierer*in bist, die*der mit Daten arbeitet, wirst du wahrscheinlich irgendwann mit JSON in Berührung kommen. In diesem Blog-Post werde ich dir zeigen, wie du mit JSON in Haskell arbeiten kannst und worauf du dabei achten solltest.

## Wie geht es

Um mit JSON in Haskell zu arbeiten, benötigst du zunächst das Paket `aeson`. Dieses Paket bietet Funktionen, mit denen du JSON-Daten parsen und serialisieren kannst. Fangen wir damit an, eine JSON-Datei zu parsen, indem wir sie in eine Haskell-Datenstruktur umwandeln. Dazu verwenden wir die Funktion `decodeFileStrict` aus dem Modul `Data.Aeson`. Angenommen, wir haben eine `Person`-Datenstruktur, die aus einem Namen und einem Alter besteht. Unsere JSON-Datei sieht folgendermaßen aus:

```Haskell
{
  "name": "Max",
  "age": 25
}
```

Um dies in Haskell zu parsen und in eine `Person`-Datenstruktur umzuwandeln, können wir folgenden Code verwenden:

```Haskell
import Data.Aeson

data Person = Person { name :: String, age :: Int }
  deriving (Show, Eq)

instance FromJSON Person where
  parseJSON (Object v) = Person <$>
    v .: "name" <*>
    v .: "age"

main :: IO ()
main = do
  maybePerson <- decodeFileStrict "person.json"
  case maybePerson of
    Nothing -> putStrLn "Error parsing JSON file."
    Just person -> print person
```

In unserem Beispiel verwenden wir die Funktion `(.:)`, um das entsprechende Feld aus dem JSON-Objekt zu extrahieren. Der Rückgabewert von `decodeFileStrict` ist vom Typ `Maybe Person`, da das Parsen fehlschlagen könnte. Wenn also alles gut geht, wird `Just person` zurückgegeben und wir können die Person einfach ausgeben.

Neben dem Parsen von JSON-Dateien können wir auch JSON-Daten in eine Datei serialisieren. Dazu verwenden wir die Funktion `encodeFile` aus dem Modul `Data.Aeson`. Angenommen, wir haben eine Liste von `Person`-Objekten und möchten sie in eine JSON-Datei schreiben. Dazu können wir folgenden Code verwenden:

```Haskell
import Data.Aeson

data Person = Person { name :: String, age :: Int }
  deriving (Show, Eq)

instance ToJSON Person where
  toJSON (Person name age) =
    object ["name" .= name, "age" .= age]

main :: IO ()
main = do
  let people = [Person "Max" 25, Person "Anna" 30]
  encodeFile "people.json" people
```

In unserem Beispiel definieren wir die Funktion `toJSON`, die ein `Person`-Objekt in ein JSON-Objekt umwandelt. Dieses wird dann mit der Funktion `object` erstellt und mit den entsprechenden Feldern versehen. Die Funktion `(.=)` weist dem jeweiligen Feld einen Wert zu.

## Tiefer Einblick

Die Arbeit mit JSON in Haskell kann manchmal etwas herausfordernd sein, da Haskell eine starke Typisierung hat und JSON-Daten oft unterschiedliche Typen haben können. Hier sind ein paar Tipps, die dir dabei helfen können:

- Um ein JSON-Feld zu extrahieren, kannst du die Funktion `(.:?)` verwenden, die den Wert des Feldes oder `Nothing`, falls das Feld nicht vorhanden ist, zurückgibt.
- Wenn du mit komplexeren JSON-Objekten arbeitest, kannst du die Funktion `fromJSON` verwenden, die eine `Value`-Variante zurückgibt. Diese kann dann mit Pattern Matching behandelt werden.
- Überprüfe immer die Rückgabewerte von Parsing- und Serialisierungsfunktionen, da sie je nach Implementierung unterschiedlich behandelt werden.

## Siehe auch

Hier sind ein paar hilfreiche Links, die dir bei der Arbeit mit JSON in Haskell helfen können:

- [Offizielle Dokumentation von `aeson`](https://hackage.haskell.org/package/aeson)
- [Tutorial zu `aeson` auf Haskell Forall](https://www.haskellforall.com/2016/04/aeson-tutorial.html)
- [Video-T