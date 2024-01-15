---
title:                "Umwandeln eines Strings in Kleinbuchstaben"
html_title:           "Go: Umwandeln eines Strings in Kleinbuchstaben"
simple_title:         "Umwandeln eines Strings in Kleinbuchstaben"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Das Konvertieren eines Strings in Kleinbuchstaben ist eine grundlegende Funktion, die häufig beim Umgang mit Texten verwendet wird. Zum Beispiel kann dies hilfreich sein, um Eingaben des Benutzers einheitlich zu formatieren oder Vergleiche zwischen Wörtern durchzuführen.

## So funktioniert es

Um einen String in Kleinbuchstaben umzuwandeln, verwendet man die Funktion `ToLower()` aus der Standardbibliothek von Go. Sie erwartet einen String als Eingabe und gibt diesen in kleingeschriebener Form zurück.

```Go
package main

import "fmt"
import "strings"

func main() {
  input := "Hallo, WELT!"
  output := strings.ToLower(input)
  fmt.Println(output) // Output: hallo, welt!
}
```

Man kann auch die Funktion `ToLower()` der `strings` Bibliothek verwenden, um alle Buchstaben in einem String in Kleinbuchstaben umzuwandeln. Diese Funktion ignoriert bereits kleingeschriebene Buchstaben.

```Go
package main

import "fmt"

func main() {
  input := "Das ist ein PATH"
  output := strings.ToLowerSpecial(unicode.TurkishCase, input)
  fmt.Println(output) // Output: das ist ein path
}
```

## Tiefere Einblicke

Go verwendet das Unicode-Sprachkonstrukt, um Zeichen zu repräsentieren. Dies bedeutet, dass die Funktion `ToLower()` alle Zeichen, die im Unicode-Standard definiert sind, in ihre entsprechenden Kleinbuchstaben umwandelt. Das Ergebnis kann daher je nach verwendeter Sprache unterschiedlich sein.

Es ist auch wichtig zu beachten, dass diese Funktion keine Zeichen in Großbuchstaben oder Sonderzeichen wie Zahlen oder Leerzeichen umwandelt.

## Siehe auch

- Offizielle Dokumentation zu `strings.ToLower()` aus der Go-Standardbibliothek: https://golang.org/pkg/strings/#ToLower
- Weitere Informationen zu Unicode in Go: https://blog.golang.org/strings