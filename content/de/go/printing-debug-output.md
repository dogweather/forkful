---
title:                "Debug-Ausgabe drucken"
html_title:           "Go: Debug-Ausgabe drucken"
simple_title:         "Debug-Ausgabe drucken"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/printing-debug-output.md"
---

{{< edit_this_page >}}

# Warum

Oftmals ist es als Programmierer wichtig, den Prozess und den Zustand eines Programms während der Ausführung zu verstehen. Das Drucken von Debug-Ausgabe kann dabei helfen, Fehler zu finden, Performance-Probleme zu lösen und das Verständnis des Codes zu verbessern.

# Wie geht das

Um Debug-Ausgabe in Go zu drucken, wird die Funktion `fmt.Printf()` verwendet. Diese Funktion nimmt als Parameter eine formatierte Zeichenkette und beliebige Werte entgegen, die in die Zeichenkette eingesetzt werden. Hier ist ein Beispiel, das den Wert einer Variablen und eine Nachricht ausgibt:

```Go
debugValue := 42
fmt.Printf("Der Wert von debugValue ist %v\n", debugValue)
```

Die Ausgabe dieses Codes wäre:

```
Der Wert von debugValue ist 42
```

Es ist auch möglich, mehrere Werte in einer Zeichenkette auszugeben, indem man mehrere Platzhalter verwendet und die entsprechenden Werte in der richtigen Reihenfolge übergeben werden. Zum Beispiel:

```Go
debugValue1 := 10
debugValue2 := "Hallo"
fmt.Printf("debugValue1: %v, debugValue2: %v\n", debugValue1, debugValue2)
```

Die Ausgabe wäre:

```
debugValue1: 10, debugValue2: Hallo
```

# Tiefer einsteigen

Die `fmt.Printf()` Funktion unterstützt auch das Formatieren der Ausgabe nach bestimmten Spezifikationen. Zum Beispiel können wir die Anzahl der Nachkommastellen bei Fließkommazahlen mit `%f` angeben und die Breite des Feldes mit `%d`.

```Go
floatVal := 3.14159265
fmt.Printf("floatVal mit 2 Nachkommastellen: %.2f", floatVal)
```

Die Ausgabe wäre:

```
floatVal mit 2 Nachkommastellen: 3.14
```

Eine vollständige Liste aller Formatierungsoptionen kann auf der offiziellen Dokumentationsseite von Go gefunden werden.

# Siehe auch

- Offizielle Go-Dokumentation zu `fmt.Printf()`: https://golang.org/pkg/fmt/#Printf
- Blog-Artikel zu Debugging mit `fmt.Printf()` in Go: https://blog.usejournal.com/debugging-the-go-way-using-fmt-9e6fd04e8efd