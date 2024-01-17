---
title:                "Umwandlung eines Strings in Kleinbuchstaben"
html_title:           "Clojure: Umwandlung eines Strings in Kleinbuchstaben"
simple_title:         "Umwandlung eines Strings in Kleinbuchstaben"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Was & Warum?
Die Umwandlung einer Zeichenkette in Kleinbuchstaben ist eine häufige Aufgabe bei der Programmierung. Sie wird verwendet, um sicherzustellen, dass alle Buchstaben in einer Zeichenkette einheitlich sind und um Vergleiche zwischen Zeichenketten durchzuführen. Programmierer machen dies oft, um die Genauigkeit von Suchvorgängen zu verbessern und um unerwünschte Fehler zu vermeiden.

# Wie geht's?
Es gibt mehrere Methoden, um eine Zeichenkette in Kleinbuchstaben umzuwandeln. In Clojure gibt es zwei Möglichkeiten, dies zu tun:

1. Die `lower-case` Funktion: Diese Funktion nimmt eine Zeichenkette als Argument und gibt eine neue Zeichenkette zurück, in der alle Buchstaben in Kleinbuchstaben umgewandelt wurden.

Clojure Code Beispiel:

```
(lower-case "HALLO WELT")
```

Output:

```
"hallo welt"
```

2. Die `to-lower-case` Funktion: Diese Funktion hat dasselbe Verhalten wie `lower-case`, kann aber auch mit anderen Datentypen wie Vektoren und Maps arbeiten.

Clojure Code Beispiel:

```
(to-lower-case "Hallo Welt")
```

Output:

```
"hallo welt"
```

# Tief eintauchen
Die Umwandlung von Zeichenketten in Kleinbuchstaben kann auf verschiedene Weise in Clojure erreicht werden. Eine alternative Methode wäre die Verwendung von regulären Ausdrücken, um alle Großbuchstaben in einer Zeichenkette zu identifizieren und in Kleinbuchstaben umzuwandeln. Dies könnte jedoch komplexer und weniger effizient sein als die Verwendung der integrierten `lower-case` und `to-lower-case` Funktionen.

# Siehe auch
- [Clojure Dokumentation für lower-case Funktion] (https://clojuredocs.org/clojure.core/lower-case)
- [Clojure Dokumentation für to-lower-case Funktion] (https://clojuredocs.org/clojure.core/to-lower-case)
- [Clojure reguläre Ausdrücke] (https://clojure.org/reference/regular_expressions)