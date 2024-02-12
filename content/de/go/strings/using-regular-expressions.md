---
title:                "Reguläre Ausdrücke verwenden"
aliases:
- /de/go/using-regular-expressions/
date:                  2024-02-03T18:11:11.270779-07:00
model:                 gpt-4-0125-preview
simple_title:         "Reguläre Ausdrücke verwenden"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Reguläre Ausdrücke (regex) werden in der Programmierung verwendet, um nach Mustern zu suchen, Übereinstimmungen zu finden und Zeichenketten basierend auf spezifischen Mustern zu manipulieren. Programmierer nutzen sie für Aufgaben, die von einfachen Validierungsprüfungen bis hin zu komplexer Textverarbeitung reichen, was sie für die flexible und effiziente Handhabung von Text unverzichtbar macht.

## Wie geht das:

In Go stellt das `regexp`-Paket die Funktionalität für reguläre Ausdrücke bereit. Hier ist eine Schritt-für-Schritt-Anleitung, wie man es benutzt:

1. **Kompilieren eines regulären Ausdrucks**

Kompilieren Sie zuerst Ihren Regex-Muster mit `regexp.Compile`. Es ist eine gute Praxis, Fehler zu behandeln, die während der Kompilierung auftreten könnten.

```go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    pattern := "go+"
    r, err := regexp.Compile(pattern)
    if err != nil {
        fmt.Println("Fehler beim Kompilieren des Regex:", err)
        return
    }
    
    fmt.Println("Regex erfolgreich kompiliert")
}
```

2. **Überprüfen von Zeichenketten**

Überprüfen Sie, ob eine Zeichenkette mit dem Muster übereinstimmt, indem Sie die Methode `MatchString` verwenden.

```go
matched := r.MatchString("goooooogle")
fmt.Println("Übereinstimmung:", matched) // Ausgabe: Übereinstimmung: true
```

3. **Finden von Übereinstimmungen**

Um die erste Übereinstimmung in einer Zeichenkette zu finden, verwenden Sie die Methode `FindString`.

```go
match := r.FindString("golang gooooo")
fmt.Println("Gefunden:", match) // Ausgabe: Gefunden: gooooo
```

4. **Finden aller Übereinstimmungen**

Für alle Übereinstimmungen nimmt `FindAllString` eine Eingabezeichenkette und eine Ganzzahl n entgegen. Wenn n >= 0, gibt es höchstens n Übereinstimmungen zurück; wenn n < 0, gibt es alle Übereinstimmungen zurück.

```go
matches := r.FindAllString("go gooo gooooo", -1)
fmt.Println("Alle Übereinstimmungen:", matches) // Ausgabe: Alle Übereinstimmungen: [go gooo gooooo]
```

5. **Ersetzen von Übereinstimmungen**

Um Übereinstimmungen durch eine andere Zeichenkette zu ersetzen, ist `ReplaceAllString` praktisch.

```go
result := r.ReplaceAllString("go gooo gooooo", "Java")
fmt.Println("Ersetzt:", result) // Ausgabe: Ersetzt: Java Java Java
```

## Tiefere Einblicke

Eingeführt in der Standardbibliothek von Go, implementiert das `regexp`-Paket die Suche nach regulären Ausdrücken und die Musterübereinstimmung, inspiriert von der Syntax von Perl. Unter der Haube kompiliert Go's Regex-Engine die Muster in eine Form von Bytecodes, die dann von einer in Go selbst geschriebenen Matching-Engine ausgeführt werden. Diese Implementierung tauscht etwas von der Geschwindigkeit, die in der direkten Hardwareausführung gefunden wird, gegen Sicherheit und Benutzerfreundlichkeit ein und vermeidet die Fallstricke von Pufferüberläufen, die in C-basierten Bibliotheken häufig sind.

Trotz seiner Leistungsfähigkeit ist Regex in Go nicht immer die optimale Lösung für Musterübereinstimmungen, insbesondere wenn es um hochstrukturierte Daten wie JSON oder XML geht. In diesen Fällen bieten spezialisierte Parser oder Bibliotheken, die für diese Datenformate konzipiert sind, eine bessere Leistung und Zuverlässigkeit. Dennoch bleibt Regex für Aufgaben, die komplizierte Textverarbeitung ohne vordefinierte Struktur beinhalten, ein wesentliches Werkzeug im Toolkit eines Programmierers und bietet eine Balance aus Leistungsfähigkeit und Flexibilität, die wenige Alternativen erreichen können.
