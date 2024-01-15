---
title:                "Die Länge eines Strings finden"
html_title:           "Go: Die Länge eines Strings finden"
simple_title:         "Die Länge eines Strings finden"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Warum

Sicherlich hast du dich schon einmal gefragt, wie du die Länge eines Strings in deinem Code abfragen kannst. Vielleicht möchtest du die Eingabe eines Nutzers überprüfen oder einfach nur die Anzahl der Zeichen in einem Wort zählen. In jedem Fall ist es wichtig zu wissen, wie du die Länge eines Strings in Go ermitteln kannst.

# How To

Um die Länge eines Strings in Go zu finden, gibt es eine einfache Funktion namens `len()`. Diese Funktion akzeptiert einen String als Argument und gibt die Anzahl der Zeichen zurück.

```Go
länge := len("Hallo Welt")
fmt.Println(länge)
```

Die obigen Zeilen deklarieren eine Variable `länge` und weisen ihr den Wert der `len()` Funktion zu, die den String "Hallo Welt" als Argument erhält. Diese Variable wird dann ausgegeben und die Ausgabe wäre `11`, da "Hallo Welt" aus 11 Zeichen besteht.

Du kannst auch die Länge eines Strings variabler machen, indem du eine Variable mit einem String als Wert durch `len()` schickst.

```Go
wort := "programmieren"
länge := len(wort)
fmt.Println(länge)
```

Die Ausgabe wäre dann `12`, da das Wort "programmieren" aus 12 Zeichen besteht (einschließlich des Leerzeichens).

# Deep Dive

Die `len()` Funktion gibt die Anzahl der Zeichen eines Strings zurück, unabhängig davon, ob es sich um Buchstaben, Zahlen, Sonderzeichen oder Leerzeichen handelt. Beachte jedoch, dass in Go jedes Zeichen 8 Bits (oder 1 Byte) belegt. Dies bedeutet, dass die Länge eines Strings nicht immer der Anzahl der Buchstaben entspricht, sondern der Anzahl der Bytes. Dies kann insbesondere dann relevant sein, wenn du mit Nicht-ASCII- oder Unicode-Text arbeitest.

# Siehe auch

- [Strings in der Go-Dokumentation](https://golang.org/ref/spec#String_types)
- [Weitere String-Manipulationsfunktionen in Go](https://www.golangprograms.com/golang-strings.html)
- [Ein interaktiver Go-Tutorial, um mehr über die Sprache zu lernen](https://tour.golang.org/welcome/1)