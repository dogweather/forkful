---
title:                "Go: Umwandlung eines Strings in Kleinbuchstaben"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Wenn Sie schon einmal mit Texten in einer Programmiersprache gearbeitet haben, wissen Sie wahrscheinlich, dass Groß- und Kleinschreibung oft wichtig ist. Manchmal möchten Sie jedoch möglicherweise alle Buchstaben in einem String in Kleinbuchstaben umwandeln, um die Vergleichbarkeit zu erhöhen oder bestimmte Operationen durchzuführen. In diesem Artikel werden wir uns ansehen, wie wir in Go eine Zeichenfolge in Kleinbuchstaben konvertieren.

## So geht's

Um in Go eine Zeichenfolge in Kleinbuchstaben umzuwandeln, können wir die `ToLower()` Funktion aus der `strings` Standardbibliothek verwenden. Hier ist ein Beispielcode, der eine Zeichenfolge mit Groß- und Kleinbuchstaben enthält und diese in eine neue Zeichenfolge mit allen Kleinbuchstaben konvertiert:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "In Go schreiben wir nur coolen Code!"

	lowerCaseStr := strings.ToLower(str)

	fmt.Println(lowerCaseStr)
}
```

Die Ausgabe dieses Codes ist: `in go schreiben wir nur coolen code!`

Wenn Sie einen Blick unter die Haube werfen, können Sie sehen, dass die `ToLower()` Funktion jedes Zeichen in der Zeichenfolge über eine Unicode-Tabelle konvertiert. Dadurch wird sichergestellt, dass auch Sonderzeichen und Symbole in Kleinbuchstaben umgewandelt werden.

## Tiefgründig

Es ist wichtig zu beachten, dass die `ToLower()` Funktion keine Zeichenfolge im Original ändert, sondern eine neue zurückgibt. Dies bedeutet, dass Sie sicherstellen müssen, dass Sie die neue Zeichenfolge einer Variablen zuweisen oder sie in einer neuen Variablen speichern, um sie verwenden zu können.

Außerdem bietet die `strings` Bibliothek noch andere nützliche Funktionen für die Bearbeitung von Zeichenfolgen, wie z.B. `ToUpper()`, `Trim()` und `Replace()`. Es lohnt sich also, sich mit den verschiedenen Funktionen vertraut zu machen, um Ihre Arbeit mit Zeichenfolgen zu erleichtern.

## Siehe auch

Hier sind einige Links zu weiterführenden Ressourcen rund um die Verwendung von Zeichenfolgen in Go:

- [Official Go documentation on strings package](https://golang.org/pkg/strings/)
- [A Beginner's Guide to Strings Manipulation in Go](https://medium.com/better-programming/a-beginners-guide-to-strings-manipulation-in-go-9b2216e77b7d)
- [Playing with Strings in Go](https://towardsdatascience.com/playing-with-strings-in-go-8baff6c43056)