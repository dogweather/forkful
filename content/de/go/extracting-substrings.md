---
title:                "Extrahieren von Teilzeichenketten"
html_title:           "Go: Extrahieren von Teilzeichenketten"
simple_title:         "Extrahieren von Teilzeichenketten"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Das Extrahieren von Teilstrings ist ein nützliches Werkzeug für jeden, der mit Zeichenketten arbeitet. Es ermöglicht das einfache Isolieren von bestimmten Teilen einer Zeichenkette, was die Verarbeitung und Analyse von Text erleichtert.

## Wie geht's

Um Teilstrings in Go zu extrahieren, verwendet man die `substring()` Funktion. Diese Funktion nimmt zwei Parameter - den Startindex und die Anzahl der zu extrahierenden Zeichen - und gibt den entsprechenden Teilstring zurück.

```Go
// Definieren einer Zeichenkette
text := "Das ist ein Beispieltext"

// Extrahieren des ersten Teilstrings (0-basiert)
teilString1 := substring(text, 0, 3)
// Output: "Das"

// Extrahieren des zweiten Teilstrings
teilString2 := substring(text, 5, 3)
// Output: "ist"

// Extrahieren des dritten Teilstrings
teilString3 := substring(text, 10, 12)
// Output: "Beispieltext"
```

Es ist auch möglich, den Endindex anstelle der Anzahl der Zeichen zu verwenden. In diesem Fall muss man den Wert des Endindex um 1 erhöhen.

```Go
text := "Das ist ein Beispieltext"

// Extrahieren des ersten Teilstrings
teilString := substring(text, 0, 3)
// Output: "Das"

// Extrahieren des zweiten Teilstrings
teilString := substring(text, 5, 8)
// Output: "ist ein"
```

## Tief einblicken

Die `substring()` Funktion basiert auf der `slice()` Operation von Go, die ähnlich wie die `substring()` Funktion arbeitet. Es ist auch möglich, Teilstrings mit der `slice()` Operation auf Arrays und Slices anzuwenden.

```Go
// Ausgangsarray
zahlen := [5]int{1, 2, 3, 4, 5}

// Extrahieren des ersten Teilstrings
teilArray := zahlen[0:2]
// Output: [1, 2]

// Extrahieren des zweiten Teilstrings
teilArray := zahlen[2:4]
// Output: [3, 4]
```

## Siehe auch

- [Go-Dokumentation zur substring()-Funktion](https://golang.org/pkg/strings/#Substring)
- [Go-Dokumentation zur slice() Operation](https://tour.golang.org/moretypes/18)