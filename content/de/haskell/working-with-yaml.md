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

## Was & Warum?
YAML ist eine einfache, menschenlesbare Datenformatierungssprache, die häufig von Programmierern verwendet wird. Sie hilft bei der Organisierung von Daten in einer strukturierten und leicht verständlichen Art und Weise. Diese Formatierung ist besonders nützlich, wenn man mit großen oder komplexen Datensätzen arbeitet.

## Wie geht's?
Um mit YAML in Haskell zu arbeiten, gibt es einige Funktionen, die dabei helfen. Hier sind zwei Beispiele, wie man YAML-Daten einlesen und ausgeben kann:
```Haskell
import qualified Data.Yaml as Y
import qualified Data.ByteString.Char8 as B

-- Einlesen von YAML-Daten aus einer Datei
Y.decodeFileThrow "beispiel.yaml" :: IO (Maybe Value)

-- Ausgabe von YAML-Daten auf der Konsole
B.putStrLn $ Y.encode $ object ["key" .= "wert"]
```
Die Ausgabe für das obige Beispiel wäre: `key: wert`

## Tiefer tauchen
YAML wurde ursprünglich von Ingy döt Net und Oren Ben-Kiki entwickelt und steht für "Yet Another Markup Language". Es wurde für die einfache Lesbarkeit von Daten entworfen und kann für eine Vielzahl von Anwendungen verwendet werden. Alternative Formate wie JSON oder XML sind oft komplexer und schwerer zu lesen.

In Haskell kann YAML mithilfe des Data.Yaml-Pakets verarbeitet werden, das verschiedene Funktionen und Typen bereitstellt, um mit YAML-Daten umzugehen. Alternativ gibt es auch andere Pakete wie YAML oder YamlConfig, die ähnliche Funktionalitäten bieten.

## Siehe auch
- [Offizielle Website von YAML](https://yaml.org/)
- [Datenformatierung mit YAML in Python (Artikel auf Englisch)](https://realpython.com/python-yaml/)
- [Haskell-Dokumentation für Data.Yaml](https://hackage.haskell.org/package/yaml/docs/Data-Yaml.html)