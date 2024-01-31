---
title:                "Anführungszeichen aus einem String entfernen"
date:                  2024-01-26T03:39:12.047268-07:00
model:                 gpt-4-0125-preview
simple_title:         "Anführungszeichen aus einem String entfernen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Anführungszeichen aus einem String zu entfernen, bedeutet, diese lästigen doppelten oder einfachen Anführungszeichen loszuwerden, die Ihren eigentlichen Text umgeben. Wir tun dies, um Daten zu bereinigen, Parsing-Fehler zu verhindern oder Text für die weitere Verarbeitung ohne den zusätzlichen Ballast von Anführungszeichen vorzubereiten.

## Wie:

Hier ist der einfache Weg, diese Anführungszeichen in Go loszuwerden:

```go
package main

import (
	"fmt"
	"strings"
)

func removeQuotes(s string) string {
	return strings.Trim(s, "'\"")
}

func main() {
	quotedString := "\"Hallo, Welt!\""
	fmt.Println("Original:", quotedString)

	unquotedString := removeQuotes(quotedString)
	fmt.Println("Ohne Anführungszeichen:", unquotedString)
}
```

Die Ausgabe sieht so aus, Anführungszeichen alle weg:

```
Original: "Hallo, Welt!"
Ohne Anführungszeichen: Hallo, Welt!
```

## Tiefergehend

Früher, als Datenformate und -austausch nicht standardisiert waren, konnten Anführungszeichen in Strings Chaos verursachen. Sie können es immer noch, insbesondere in JSON oder beim Einpflegen von Strings in Datenbanken. Das `strings`-Paket in Go kommt mit einer `Trim`-Funktion, die nicht nur Leerzeichen, sondern auch alle Zeichen, die Sie nicht mögen, wegzaubert.

Warum nicht Regex? Nun, `Trim` ist schneller für einfache Aufgaben, aber wenn Ihre Strings mit Anführungszeichen an seltsamen Stellen Verstecken spielen, könnte regex Ihre schwere Artillerie sein:

```go
import "regexp"

func removeQuotesWithRegex(s string) string {
	re := regexp.MustCompile(`^["']|["']$`)
	return re.ReplaceAllString(s, "")
}
```

Es ist wie die Wahl zwischen Schere und Kettensäge; wählen Sie das Tool, das für den Job geeignet ist.

## Siehe Auch

Für mehr über das `strings`-Paket und seine Power-Tools:
- [Paket strings](https://pkg.go.dev/strings)

Um die Macht von regulären Ausdrücken in Go zu nutzen:
- [Paket regexp](https://pkg.go.dev/regexp)

Möchten Sie in die Philosophie des String-Bereinigens eintauchen?
- [Die Trim-Methode](https://blog.golang.org/strings)
