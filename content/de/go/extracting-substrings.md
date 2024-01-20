---
title:                "Teilzeichenketten extrahieren"
html_title:           "PowerShell: Teilzeichenketten extrahieren"
simple_title:         "Teilzeichenketten extrahieren"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/extracting-substrings.md"
---

{{< edit_this_page >}}

# Eine Einführung in das Extrahieren von Teilstrings in Go

## Was & Warum?

Das Extrahieren von Teilstrings, auch **Teilzeichensätze** genannt, ist die Tätigkeit, einen Teil eines Strings zu isolieren. Es ist ein häufiges Werkzeug in der Programmierung, um spezifische Daten zu manipulieren oder zu analysieren.

## Wie man es macht:

In Go können wir den Slicing-Operator `[:]` zur Extraktion von Teilstrings verwenden. Hier ist ein einfaches Beispiel:

```Go
package main
import "fmt"

func main() {
    str := "Guten Morgen, Welt!"
    teilstring := str[6:12]
    fmt.Println(teilstring)
}
```

Die Ausgabe wäre `Morgen`. Beachten Sie, dass der Bereich bis zum Ende des Index, den Sie angegeben haben, jedoch nicht einschließlich ist.

## Vertiefung

Historisch gesehen wurden Teilstrings in vielen Fällen zur Zeichenkettenmanipulation verwendet, nicht nur in Go, sondern auch in anderen Programmiersprachen.

Als Alternative kannst du die `strings`-Bibliothek in Go verwenden, um die `Split`- oder `Fields`-Funktionen zu nutzen. Diese Funktionen sind nützlich, wenn du Teilstrings basierend auf einem Trennzeichen extrahieren möchtest.

Während das `[Start:Ende]` Slicing in Go effizient ist, ist es wichtig zu wissen, dass Go die Zeichen mithilfe von UTF-8 kodiert. Daher kann der Versuch, einen Teilstring aus einem multibyte-Zeichen zu extrahieren, zu unerwarteten Ergebnissen führen.

## Sieh auch

* [Offizielle Go-Dokumentation](https://golang.org/doc/): Die umfangreiche Dokumentation zu Go, einschließlich einer Einführung und ausführlichen Erläuterungen zu allen Aspekten der Sprache.

* [Go-Schnitzel-Tutorial](https://blog.golang.org/slices-intro): Ein gründliches Tutorial über das Slice-Konzept in Go, das auch das Slicing von Strings abdeckt.

* [Go Strings, bytes package](https://golang.org/pkg/strings): Die offizielle Go-Dokumentation für das `strings` und `bytes` Package, das viele nützliche Funktionen für die Arbeit mit Strings bereitstellt. 

Bleiben Sie beim Coden flexibel und lassen Sie sich nicht von Herausforderungen abschrecken!