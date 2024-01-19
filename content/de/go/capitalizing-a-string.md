---
title:                "Einen String großschreiben"
html_title:           "Go: Einen String großschreiben"
simple_title:         "Einen String großschreiben"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Kapitalisieren eines Strings in der Programmierung beschreibt das Umwandeln aller Anfangsbuchstaben eines Textes in Großbuchstaben. Programmeure verwenden es, um Inhalte für den Benutzer lesbar und ästhetisch ansprechend zu machen.

## Wie zu:

Im Folgenden finden Sie Codebeispiele, die zeigen, wie man einen String in Go kapitalisiert. 

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	text := "hallo, Welt!"
	fmt.Println(strings.Title(text)) // Ausgabe: Hallo, Welt!
}
```

In diesem Code benutzen wir die `Title`-Funktion aus dem `strings`-Paket, um den ersten Buchstaben jedes Worts im String in einen Großbuchstaben umzuwandeln.

## Vertiefung:

Die Methode zur Kapitalisierung von Strings stammte ursprünglich von älteren Computersprachen, die standardmäßig keine Funktion zur Umwandlung von Buchstaben in Großbuchstaben besitzen. Obwohl Go ein `strings`-Paket enthält, das die `Title`-Funktion aufweist, kann man auch eigene Funktionen erstellen.

Man kann sogar Text in UNICODE zu kapitalisieren, indem man den Codepunkt jedes Buchstabens prüft und ihn von Klein- auf Großbuchstaben umwandelt, falls er ein Kleinbuchstabe ist.

```Go
package main

import (
	"fmt"
	"unicode"
)

func main() {
	text := "hallo, Welt!"
	fmt.Println(strings.Map(unicode.ToTitle, text)) // Ausgabe: HALLO, WELT!
}
```

## Siehe auch:

- Go Standard Library: strings (https://golang.org/pkg/strings/)
- Go by Example: String Functions (https://gobyexample.com/string-functions) 

Erfahren Sie mehr über die Go-Standardbibliothek und die Möglichkeiten der String-Manipulation, um die Effizienz und Leistung Ihrer Go-Programme zu verbessern.