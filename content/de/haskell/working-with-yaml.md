---
title:                "Haskell: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

# Warum

Haskell ist eine Programmiersprache, die für ihre Eleganz, Strenge und Ausdrucksstärke bekannt ist. Wenn Sie also auf der Suche nach einer effizienten und klar strukturierten Methode zum Verarbeiten von Daten sind, ist Haskell die richtige Wahl. In diesem Blogbeitrag werden wir uns damit beschäftigen, wie wir YAML-Dateien verwenden können, um unsere Daten in Haskell zu verarbeiten.

# Wie Gehe Ich Vor

Um mit YAML in Haskell zu arbeiten, müssen Sie zunächst das "yaml" Paket importieren. Anschließend können Sie eine YAML-Datei in Ihrem Code öffnen und die Daten in ein passendes Format konvertieren. Hier ist ein Beispiel:

```Haskell
import Data.Yaml

main :: IO ()
main = do
    -- Datei öffnen
    file <- readFile "beispiel.yaml"
    -- Daten in das passende Format konvertieren
    let result = Data.Yaml.decode file :: Maybe [Int]
    -- Ausgabe
    case result of
        Just numbers -> print numbers
        Nothing -> print "Datei konnte nicht richtig gelesen werden."
```

In diesem Beispiel verwenden wir die "decode" Funktion aus dem "yaml" Paket, um die Daten in ein "Maybe"-Typ umzuwandeln. Das "Maybe" ist notwendig, da die YAML-Datei sowohl Zeichenketten als auch Nummern enthalten kann und wir uns nicht sicher sein können, welche Art von Daten wir erhalten werden.

Die Ausgabe wird in diesem Fall eine Liste von Zahlen sein, wie in der YAML-Datei angegeben. Beachten Sie, dass, falls die Datei nicht richtig gelesen werden kann, unsere Anwendung eine entsprechende Fehlermeldung ausgeben wird.

# Tiefer Eintauchen

Eine wichtige Sache, die man bei der Arbeit mit YAML in Haskell beachten sollte, ist die Verwendung von benutzerdefinierten Typen. Um die Daten in eine benutzerdefinierte Struktur zu konvertieren, müssen Sie die "FromJSON"-Instanz der entsprechenden Typklasse für Ihren Datentyp definieren. Hier ist ein Beispiel:

```Haskell
data Person = Person
    { name :: String
    , age :: Int
    , occupation :: String
    } deriving Show

instance FromJSON Person where
    parseJSON (Object v) = Person <$> v .: "name" <*> v .: "age" <*> v .: "occupation"
    parseJSON _ = mzero
```

In diesem Beispiel haben wir einen benutzerdefinierten Datentyp namens "Person" definiert und eine Instanz der "FromJSON"-Klasse erstellt, die die Konvertierung von YAML-Daten in ein "Person"-Objekt ermöglicht. Beachten Sie, wie wir die "object" und "v .:" Funktionen verwenden, um die entsprechenden Felder aus der YAML-Datei auszulesen und in unsere "Person"-Struktur einzufügen.

# Siehe Auch

- [Haskell Dokumentation zum YAML-Paket](https://hackage.haskell.org/package/yaml)
- [Haskell-Kurs auf Codecademy](https://www.codecademy.com/learn/learn-haskell) (Englisch)
- [YAML 1.2 Spezifikation](http://yaml.org/spec/1.2/spec.html) (Englisch)