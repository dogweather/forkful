---
title:                "Zeichenketten verbinden"
html_title:           "Go: Zeichenketten verbinden"
simple_title:         "Zeichenketten verbinden"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich mit der Verkettung von Zeichenketten beschäftigen? Nun, Zeichenketten sind in der Programmierung ein häufig verwendetes Konzept, das es uns ermöglicht, Texte zu manipulieren und zu formatieren. Die Verkettung von Zeichenketten ermöglicht es uns, mehrere Zeichenketten miteinander zu verbinden, um so komplexe Ausgaben zu erzeugen.

## Wie geht's?

Um Zeichenketten in Go zu verkettung, können wir den `+` Operator verwenden oder die `fmt.Sprintf()` Funktion nutzen. Hier sind zwei Beispiele, die das Verkettung von Zeichenketten demonstrieren:

```go
// Verwendung des `+` Operators
string1 := "Hallo"
string2 := "Welt"
result := string1 + " " + string2
fmt.Println(result) // gibt "Hallo Welt" aus

// Verwendung von `fmt.Sprintf()`
string3 := "Mein Name ist"
string4 := "Go"
result := fmt.Sprintf("%s %s", string3, string4) 
fmt.Println(result) // gibt "Mein Name ist Go" aus
```

Die `+` Operator Methode ist gut geeignet für kurze und einfache Verkettungen, während die `fmt.Sprintf()` Funktion bei komplexeren Szenarien bevorzugt wird. Beachte, dass wir in der letzten Zeile der Printfunktion die formatierten Platzhalter `%s` verwenden, um unsere Variablen in die Ausgabe einzufügen.

## Tiefentauchen

Nun, da wir wissen, wie man Zeichenketten in Go verkettet, werfen wir einen Blick auf die Hintergründe dieses Konzepts. In der Programmierung werden Zeichenketten als Datenstruktur verwendet, um Texte zu speichern und zu manipulieren. Diese sind in Go immutable, das bedeutet, dass sie nicht geändert werden können, sobald sie erstellt wurden. Daher ist die Verkettung von Zeichenketten ein Prozess, bei dem eine neue Zeichenkette erzeugt wird, indem mehrere Zeichenketten zusammengefügt werden. Dies kann eine zusätzliche Speicherbelastung verursachen, daher ist es wichtig, die Verkettung sparsam und effizient zu nutzen.

## Siehe auch

Hier sind einige nützliche Links, die dir helfen können, deine Verkettungsfähigkeiten in Go zu verbessern:

- [Offizielle Go-Dokumentation zum Verkettung von Zeichenketten](https://golang.org/pkg/strings/)
- [Ein ausführlicher Artikel über die Manipulation von Zeichenketten in Go](https://www.calhoun.io/concatenating-and-manipulating-strings-in-go/)