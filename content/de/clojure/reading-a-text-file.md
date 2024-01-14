---
title:    "Clojure: Eine Textdatei lesen"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

In der Welt der Programmierung ist das Lesen von Textdateien ein grundlegender und notwendiger Schritt für die Verarbeitung von Daten, sei es zum Lesen von Konfigurationsdateien oder zum Extrahieren von Informationen aus großen Datenmengen. Lesen von Textdateien ermöglicht es einem Programmierer, auf einfache Weise Informationen aus einem externen Quelltext zu erhalten, ohne die Daten manuell eingeben zu müssen.

## Wie man Textdateien in Clojure liest

Das Lesen von Textdateien in Clojure ist relativ einfach und erfordert nur wenige Zeilen Code. In der folgenden Beispielcode verwenden wir die Funktion `slurp` um den Inhalt einer Textdatei in eine variable zu speichern und dann die `println` Funktion, um den Inhalt auf der Konsole auszugeben.

```Clojure
(def text (slurp "datei.txt"))
(println text)
```

Die Ausgabe wird dann den gesamten Inhalt der Textdatei auf der Konsole ausgeben. Alternativ kann auch die Funktion `read-lines` verwendet werden, um den Inhalt einer Textdatei zeilenweise in eine Liste zu speichern.

```Clojure
(def lines (read-lines "datei.txt"))
(println lines)
```

Die Ausgabe wird dann eine Liste mit jeder Zeile der Textdatei als separate Elemente enthalten.

## Tiefere Einblicke

Es gibt auch Möglichkeiten, Textdateien in Clojure zu lesen, die über die grundlegenden Funktionen `slurp` und `read-lines` hinausgehen. Zum Beispiel kann die Bibliothek "clojure-csv" verwendet werden, um CSV-Dateien zu lesen und in Clojure Datenstrukturen zu konvertieren.

Eine weitere Möglichkeit ist die Verwendung von regulären Ausdrücken, um spezifische Informationen aus einer Textdatei zu extrahieren. In Kombination mit den Funktionen `slurp` und `read-lines` können reguläre Ausdrücke eingesetzt werden, um gezielt nach bestimmten Mustern in einer Textdatei zu suchen und diese zu verarbeiten.

## Siehe auch

- [Offizielle Clojure Dokumentation](https://clojure.org/)
- [Clojure Cheat Sheet](https://clojure.org/api/cheatsheet)
- [Clojure for Beginners YouTube Kanal](https://www.youtube.com/channel/UCbwhWannAwBT7PcDPKnox-g)