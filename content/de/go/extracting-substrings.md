---
title:    "Go: Unterstrings extrahieren"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Das Extrahieren von Teilstrings ist eine häufige Aufgabe in der Programmierung, die wir alle irgendwann einmal machen müssen. Es kann hilfreich sein, wenn wir nur einen Teil einer Zeichenkette benötigen oder wenn wir bestimmte Informationen analysieren möchten. Glücklicherweise bietet die Go-Programmiersprache eingebaute Funktionen, um diese Aufgabe einfach und effizient zu erledigen.

## Wie man es macht

Die `strings`-Bibliothek von Go enthält Funktionen wie `Substr` und `Split`, die beim Extrahieren von Teilstrings aus einem String helfen. Hier ist ein Beispiel für die Verwendung von `Split`:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	myString := "Dies ist ein Beispieltext."
	substring := strings.Split(myString, " ")
	fmt.Println(substring)
}
```

Dieses Beispiel wird das Array `["Dies", "ist", "ein", "Beispieltext."]` ausgeben, indem es den String an jeder Leerstelle aufteilt. Alternativ dazu gibt es die `Substr`-Funktion, die es uns ermöglicht, einen bestimmten Teil des Strings basierend auf Indexpositionen zu extrahieren. Hier ist ein Beispiel:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	myString := "Dies ist ein Beispieltext."
	substring := strings.Substr(myString, 5, 8)
	fmt.Println(substring)
}
```

Dieses Beispiel wird `"ist"` ausgeben, indem es den Teil des Strings von Index 5 bis Index 8 (nicht inklusive) extrahiert.

## Tiefergehende Einblicke

Wenn wir uns genauer mit der `strings`-Bibliothek beschäftigen, finden wir weitere Funktionen, die beim Extrahieren von Teilstrings hilfreich sein können. Beispielsweise gibt es die Funktion `Index`, die uns die Indexposition eines bestimmten Teilstrings innerhalb eines Strings gibt. Es gibt auch `Contains`, das überprüft, ob ein String einen bestimmten Teilsubstring enthält, und `Replace`, das Teile eines Strings durch andere ersetzen kann.

In der Praxis gibt es viele verschiedene Situationen, in denen das Extrahieren von Teilstrings nützlich sein kann, und die Go-Sprache bietet uns die Werkzeuge, um dies einfach zu tun.

## Siehe auch

- [Learn Go: Strings](https://learn.go.dev/strings)
- [A Tour of Go: Strings](https://tour.golang.org/basics/5)
- [Go By Example: String Functions](https://gobyexample.com/string-functions)