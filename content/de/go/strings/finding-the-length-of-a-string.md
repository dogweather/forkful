---
title:                "Die Länge einer Zeichenkette ermitteln"
date:                  2024-02-03T17:56:43.657527-07:00
model:                 gpt-4-0125-preview
simple_title:         "Die Länge einer Zeichenkette ermitteln"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/finding-the-length-of-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?
Die Länge eines Strings in Go zu finden, bedeutet, die Anzahl der Zeichen zu bestimmen, die er enthält. Programmierer führen diese Operation routinemäßig durch, um Strings effektiv zu manipulieren, sei es zur Validierung, zum Extrahieren von Teilstrings oder einfach, um Einschränkungen bei Benutzereingaben durchzusetzen.

## Wie zu:
In Go werden Strings als unveränderliche Byte-Sequenzen behandelt. Sie können die Länge eines Strings mit der eingebauten Funktion `len()` finden, die die Anzahl der Bytes zurückgibt, nicht unbedingt die Anzahl der Zeichen. So verwenden Sie es:

```go
package main

import (
	"fmt"
	"unicode/utf8"
)

func main() {
	// Verwendung von len(), um die Byte-Länge zu finden
	str := "Hello, 世界"
	byteLength := len(str)
	fmt.Println("Byte-Länge:", byteLength) // Ausgabe: Byte-Länge: 13

	// Um die genaue Anzahl der Zeichen oder Runen in einem String zu erhalten
	runeLength := utf8.RuneCountInString(str)
	fmt.Println("Runen-Länge:", runeLength) // Ausgabe: Runen-Länge: 9
}
```
Die erste Methode, die `len()` verwendet, liefert möglicherweise nicht immer das erwartete Ergebnis, da sie Bytes zählt. Für Strings, die Nicht-ASCII-Zeichen enthalten (wie "世界"), sollte stattdessen `RuneCountInString` aus dem Paket `unicode/utf8` verwendet werden, um Unicode-Codepunkte genau zu zählen.

## Tiefgang
Vor Go 1 gab es keine strikte Abgrenzung für die Behandlung von Strings als Byte-Sequenzen versus Zeichen-Sequenzen. Nach Go 1 führte die Übernahme von UTF-8 als Standard-Codierungsschema für Strings zu klareren Ansätzen. Die Funktion `len()` funktioniert perfekt für ASCII-Strings, bei denen Zeichen in einem einzigen Byte dargestellt werden. Doch als Go-Anwendungen globaler wurden und der Bedarf, eine Fülle von Sprachen und Zeichensätzen zu unterstützen, wuchs, zeigten die simpel Ansätze von `len()` Grenzen.

Die Einführung und Verwendung von `utf8.RuneCountInString()` beantworten diese Grenzen, indem sie eine Möglichkeit bieten, tatsächliche Unicode-Zeichen (Runen in der Go-Terminologie) zu zählen. Diese Methode stellt sicher, dass die Längenberechnung unabhängig von den Kodierungsspezifikationen von UTF-8 ist, wo Zeichen mehrere Bytes umfassen können.

Ein alternativer Ansatz für das Durchlaufen und Manipulieren von Strings, der mehr im Einklang mit Gos Konkurrenz- und Effizienzethos steht, könnte darin bestehen, Strings als Slices von Runen zu behandeln. Dieser Ansatz erfordert jedoch einen Konvertierungsschritt und löst nicht sofort alle Feinheiten von Unicode (z. B. kombinierende Zeichen).

Zusammenfassend ist, während `len()` für die Byte-Länge geeignet und für ASCII-Text effizient ist, `utf8.RuneCountInString()` eine zuverlässigere Wahl für eine global kompatible Anwendung. Dennoch werden Entwickler ermutigt, die Kompromisse in Leistung und Speichernutzung zu verstehen, die diese Wahlen mit sich bringen.
