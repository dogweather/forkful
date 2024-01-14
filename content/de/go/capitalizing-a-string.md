---
title:                "Go: String in Großbuchstaben umwandeln"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich in Go mit dem Großschreiben von Zeichenketten beschäftigen? Nun, diese Funktion ist oft in der Softwareentwicklung erforderlich, beispielsweise beim Validieren von Benutzereingaben oder beim Erstellen von Datenbankabfragen.

## Wie man es macht

Um eine Zeichenkette in Go zu versalzen, gibt es mehrere Möglichkeiten. Eine einfache Möglichkeit ist die Verwendung der `strings` Bibliothek und der `ToUpper()` Funktion, wie im folgenden Beispiel gezeigt:

```Go
import "strings"

fmt.Println(strings.ToUpper("hallo dort")) // Ausgabe: HALLO DORT
```

Eine weitere Möglichkeit ist die Verwendung von Regex-Ausdrücken mit Hilfe der `regexp` Bibliothek:

```Go
import "regexp"

// Muster, um nur die ersten beiden Buchstaben einer Zeichenkette zu großschreiben
pattern := regexp.MustCompile(`^([a-z]{2})`)
fmt.Println(pattern.ReplaceAllStringFunc("hallo dort", strings.ToUpper)) // Ausgabe: HALlo dort
```

Wie man sehen kann, gibt es verschiedene Wege, um eine Zeichenkette in Go zu versalzen. Die Wahl hängt davon ab, in welchem Kontext und welcher Komplexität die Funktion verwendet werden soll.

## Tiefen-Tauchgang

Wenn es um das Versalzen von Zeichenketten in Go geht, ist es wichtig zu wissen, dass Go standardmäßig UTF-8 verwendet. Das bedeutet, dass Zeichenketten auch Unicode-Zeichen enthalten können, die gegebenenfalls beim Versalzen berücksichtigt werden müssen.

Eine andere wichtige Sache ist die Leistung. Wenn es in einem Programm erforderlich ist, viele Zeichenketten zu versalzen, kann es sinnvoll sein, einen schnelleren Ansatz zu wählen, z.B. durch Direktzugriff auf die Bytes der Zeichenkette anstatt auf String-Funktionen zurückzugreifen.

## Siehe auch

- [Offizielle Dokumentation zu `strings` in Go](https://pkg.go.dev/strings)
- [Offizielle Dokumentation zu `regexp` in Go](https://pkg.go.dev/regexp)
- [Tutorial über Unicode in Go](https://www.calhoun.io/unicode-tutorial-in-go/)