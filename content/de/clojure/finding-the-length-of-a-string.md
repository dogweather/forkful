---
title:                "Clojure: Die Länge eines Strings finden."
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Finden der Länge eines Strings ist eine grundlegende Aufgabe in der Programmierung. Es ist besonders hilfreich, wenn man mit Texten arbeitet, da man oft wissen muss, wie viele Zeichen ein String enthält. In diesem Blog-Beitrag werden wir untersuchen, wie man die Länge eines Strings in Clojure berechnen kann.

## Wie man String Länge in Clojure findet

Eine Möglichkeit, die Länge eines Strings in Clojure zu finden, ist die Verwendung der Funktion `count`. Diese Funktion zählt die Anzahl der Elemente in einem Collection-Datentyp. Da Strings in Clojure als Collection von Zeichen behandelt werden, können wir die `count`-Funktion verwenden, um die Länge eines Strings zu erhalten. Hier ist ein Beispiel:

```Clojure
(def string "Guten Tag")
(count string)
```

Das Ergebnis wäre `9`, da der String "Guten Tag" 9 Zeichen hat. Wir können auch direkt einen String als Argument an die `count`-Funktion übergeben:

```Clojure
(count "Hallo")
```

Das gibt ebenfalls das Ergebnis `5` zurück.

## Tief eintauchen

Es ist wichtig zu beachten, dass die `count`-Funktion nicht nur für Strings verwendet werden kann, sondern für alle Collection-Datentypen. Das bedeutet, dass wir nicht nur die Länge von Strings, sondern auch von Vektoren, Listen oder Maps finden können.

Wir können auch die `clojure.string` Bibliothek verwenden, um die Länge eines Strings zu berechnen. Diese Bibliothek enthält die Funktion `length`, die ähnlich wie `count` funktioniert. Hier ist ein Beispiel der Verwendung von `length`:

```Clojure
(require '[clojure.string :as str])
(def string "Hallo")
(str/length string)
```

Das Ergebnis wäre `5`. Eine andere hilfreiche Funktion in der `clojure.string` Bibliothek ist `trim`, die es uns ermöglicht, Leerzeichen am Anfang und Ende eines Strings zu entfernen, bevor wir die Länge berechnen.

## Siehe auch

- [Dokumentation für `count` Funktion](https://clojuredocs.org/clojure.core/count)
- [Dokumentation für `clojure.string` Bibliothek](https://clojuredocs.org/clojure.string)
- [Weitere Ressourcen für Clojure Programmierer (auf Deutsch)](https://www.clojure.org/resources)