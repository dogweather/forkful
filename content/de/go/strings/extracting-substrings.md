---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:56:37.060729-07:00
description: "Das Extrahieren von Teilzeichenketten beinhaltet das Abrufen spezifischer\
  \ Teile einer Zeichenkette basierend auf ihren Positionen. Programmierer f\xFChren\u2026"
lastmod: '2024-03-13T22:44:53.275269-06:00'
model: gpt-4-0125-preview
summary: Das Extrahieren von Teilzeichenketten beinhaltet das Abrufen spezifischer
  Teile einer Zeichenkette basierend auf ihren Positionen.
title: Teilstrings extrahieren
weight: 6
---

## Was & Warum?

Das Extrahieren von Teilzeichenketten beinhaltet das Abrufen spezifischer Teile einer Zeichenkette basierend auf ihren Positionen. Programmierer führen diese Operation häufig durch, um Textdaten effizient zu verarbeiten oder zu manipulieren, wie zum Beispiel das Parsen von Eingaben, das Validieren von Formaten oder das Vorbereiten von Ausgaben.

## Wie:

In Go ist der `string` Typ ein schreibgeschützter Slice von Bytes. Um Teilzeichenketten zu extrahieren, verwendet man hauptsächlich die `slice` Syntax, zusammen mit der integrierten `len()` Funktion für die Längenprüfung und dem `strings` Paket für komplexere Operationen. So können Sie dies erreichen:

### Grundlegendes Slicing

```go
package main

import (
    "fmt"
)

func main() {
    str := "Hello, World!"
    // Extrahiert "World"
    subStr := str[7:12]
    
    fmt.Println(subStr) // Ausgabe: World
}
```

### Verwendung des `strings` Pakets

Für fortgeschrittenere Teilzeichenketten-Extraktionen, wie das Extrahieren von Zeichenketten nach oder vor einer spezifischen Teilzeichenkette, können Sie das `strings` Paket verwenden.

```go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "name=John Doe"
    // Extrahiert Teilzeichenkette nach "="
    subStr := strings.SplitN(str, "=", 2)[1]
    
    fmt.Println(subStr) // Ausgabe: John Doe
}
```

Es ist wichtig zu beachten, dass Go-Zeichenketten UTF-8 kodiert sind und ein direkter Byte-Slice nicht immer in gültigen Zeichenketten resultiert, wenn sie Mehrbyte-Zeichen enthalten. Für die Unterstützung von Unicode ziehen Sie die Verwendung von `range` oder das `utf8` Paket in Betracht.

### Umgang mit Unicode-Zeichen

```go
package main

import (
    "fmt"
    "unicode/utf8"
)

func main() {
    str := "Hello, 世界"
    // Findet Teilzeichenkette unter Berücksichtigung von Unicode-Zeichen
    runeStr := []rune(str)
    subStr := string(runeStr[7:])
    
    fmt.Println(subStr) // Ausgabe: 世界
}
```

## Tiefere Einblicke

Das Extrahieren von Teilzeichenketten in Go ist dank seiner Slice-Syntax und umfangreichen Standardbibliothek unkompliziert. Historisch gesehen boten frühere Programmiersprachen direktere Funktionen oder Methoden für solche Textmanipulationen. Gleichwohl betont Golang Sicherheit und Effizienz, insbesondere mit seinen unveränderlichen Zeichenketten und der expliziten Handhabung von Unicode-Zeichen durch Runen.

Während grundlegendes Slicing von der Leistungseffizienz profitiert, erbt es die Komplexitäten der direkten Handhabung von UTF-8 Zeichen. Die Einführung des `rune` Typs erlaubt es Go-Programmen, sicher mit Unicode-Text umzugehen, was es zu einer leistungsstarken Alternative für internationale Anwendungen macht.

Darüber hinaus könnten Programmierer aus anderen Sprachen integrierte High-Level-String-Manipulationsfunktionen vermissen. Doch bieten die `strings` und `bytes` Pakete in Golangs Standardbibliothek einen reichen Satz an Funktionen, die, während sie etwas mehr Boilerplate benötigen, mächtige Optionen für die Stringverarbeitung bieten, einschließlich des Extrahierens von Teilzeichenketten.

Im Wesentlichen spiegeln die Designentscheidungen von Go rund um die Zeichenkettenmanipulation seine Ziele für Einfachheit, Leistung und Sicherheit im Umgang mit modernen, internationalisierten Textdaten wider. Während es eine leichte Anpassung erfordern könnte, bietet Go wirksame und effiziente Werkzeuge für die Handhabung von Teilzeichenkettenextraktion und mehr.
