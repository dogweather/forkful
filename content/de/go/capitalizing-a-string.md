---
title:                "String in Großbuchstaben umwandeln"
date:                  2024-01-19
html_title:           "C: String in Großbuchstaben umwandeln"
simple_title:         "String in Großbuchstaben umwandeln"

category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Das Großschreiben eines Strings bedeutet, alle Zeichen im String in Großbuchstaben zu konvertieren. Es wird oft für die Vereinheitlichung von Nutzereingaben oder für das Anzeigen von Text in einer standardisierten Formatierung verwendet.

## How to:
In Go kannst du den String mit der `ToUpper` Funktion aus dem `strings` Paket großschreiben. Hier ist ein kurzes Beispiel:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	original := "hallo, welt!"
	capitalized := strings.ToUpper(original)
	fmt.Println(capitalized)
}
```

Ausgabe:

```
HALLO, WELT!
```

## Deep Dive
Das Großschreiben von Strings existiert seit den Anfängen der Textverarbeitung. In Go erfolgt dies durch die `ToUpper` Funktion im `strings` Paket, die Unicode-zeichenweise operiert. 

Alternativen zur Standardmethode `ToUpper` sind:
- `ToUpperSpecial` für kulturspezifische Großschreibung
- Manuelle Iteration über den String und Anwendung der `unicode.ToUpper` Funktion auf jedes Zeichen

Bei der Implementierung sollte bedacht werden, dass nicht alle Sprachen das Konzept von Groß- und Kleinbuchstaben wie im lateinischen Alphabet handhaben. Außerdem müssen Sonderfälle wie deutsche Umlaute (`ä`, `ö`, `ü`) und das `ß`, das zu `SS` wird, korrekt umgesetzt werden. Die Standardbibliothek von Go handhabt diese Eigenheiten bereits.

## See Also
- Go-Strings-Paket: https://golang.org/pkg/strings/
- Unicode-Paket in Go: https://golang.org/pkg/unicode/
- Diskussion über Zeichen und ihre Groß-/Kleinschreibung: https://blog.golang.org/strings
