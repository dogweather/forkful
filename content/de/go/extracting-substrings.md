---
title:    "Go: Extrahieren von Teilstrings"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Warum

Die Extraktion von Teilstrings ist ein grundlegender Teil der Textmanipulation in der Programmierung. Diese Funktion ermöglicht es uns, bestimmte Teile eines Textes zu isolieren und damit weiter zu arbeiten, ohne den ursprünglichen String zu verändern. Dies kann in verschiedenen Anwendungsfällen nützlich sein, wie zum Beispiel bei der Verarbeitung von Benutzereingaben oder beim Parsen von Daten.

## So geht's

Um Teilstrings in Go zu extrahieren, gibt es mehrere Möglichkeiten. Eine davon ist die Verwendung der Standardbibliotheksfunktion `substring()`. Diese Funktion nimmt als Parameter den ursprünglichen String sowie die Start- und Endposition des gewünschten Teilstrings an und gibt diesen zurück. Sehen wir uns ein Beispiel an:

```Go
text := "Hallo, wie geht es dir?"
result := substring(text, 7, 17)
fmt.Println(result)
```

Dieses Beispiel würde den Teilstring "wie geht es" aus dem ursprünglichen String extrahieren und ausgeben.

Eine weitere Möglichkeit ist die Verwendung von regulären Ausdrücken mit der `regexp`-Bibliothek. Diese Methode bietet mehr Flexibilität bei der Spezifizierung des Teilstrings, da man auch Muster angeben kann, nach denen gesucht werden soll. Hier ist ein Beispiel:

```Go
pattern := "Wie ([a-z]+) es"
regexp := regexp.MustCompile(pattern)
matches := regexp.FindStringSubmatch(text)
fmt.Println(matches[1])
```

Dieses Beispiel würde ebenfalls den Teilstring "wie geht" aus dem ursprünglichen String extrahieren und ausgeben.

## Tief tauchen

Wenn man tiefer in die Extraktion von Teilstrings einsteigen möchte, kann man sich mit den verschiedenen Stringmanipulationsfunktionen in Go beschäftigen. Dazu gehören unter anderem `split()`, `join()` und `replace()`, die alle nützlich sein können, um spezifische Teilstrings zu isolieren und zu verändern. Auch die genaue Handhabung von Regulären Ausdrücken ist ein wichtiges Thema, um komplexe Teilstrings zu extrahieren.

## Siehe auch

- [Go Standardbibliotheksdokumentation zur Substring-Funktion](https://golang.org/pkg/strings/#NewReader)
- [Go Regular Expressions Tutorial](https://www.geeksforgeeks.org/how-to-match-regular-expressions-in-go/)