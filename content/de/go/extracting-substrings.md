---
title:                "Go: Substringextraktion"
simple_title:         "Substringextraktion"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Substring-Extraktion ist ein häufig verwendetes Konzept in der Go-Programmierung. Es ermöglicht uns, einen Teil eines Strings zu extrahieren, der für unsere Anwendung relevant ist. In diesem Blog-Beitrag werden wir uns genauer mit diesem Konzept beschäftigen und sehen, wie es in unseren Programmen verwendet werden kann.

## Wie geht das

Die String-Extraktion in Go ist sehr einfach und intuitiv. Wir können die `substring`-Funktion aus dem `strings`-Paket verwenden, um einen Teil eines Strings zu extrahieren. Hier ist ein kleines Beispiel, wie wir einen Teil eines Strings extrahieren können:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	text := "Gophers sind großartig!"
	substring := text[8:14]
	fmt.Println(substring)
}

```

Die Ausgabe dieses Codes wird `sind g` sein, da wir die Zeichen von der 8. Position bis zur 14. Position extrahieren. Wenn wir nur den Startindex angeben, wird der Rest des Strings bis zum Ende extrahiert. Hier ist ein weiteres Beispiel:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	text := "Hello, world!"
	substring := text[7:]
	fmt.Println(substring)
}

```

Die Ausgabe wird `world!` sein, da wir von der 7. Position bis zum Ende des Strings extrahieren. Wir können auch den Endindex weglassen, um den gesamten String ab einem bestimmten Index zu extrahieren.

## Tiefer eintauchen

Die `substring`-Funktion in Go basiert auf der `slice`-Syntax, die verwendet wird, um Teile von Arrays oder Slices zu extrahieren. In der `slice`-Syntax gibt der Startindex an, wo das Teil-Array / der Slice beginnen soll, und der Endindex gibt an, wo das Teil-Array / der Slice enden soll. Beachten Sie, dass der Endindex nicht einschließlich des Elements an dieser Position ist. Dies ist auch der Fall bei der Verwendung der `substring`-Funktion.

## Siehe auch

- [Golang.org Strings-Paket](https://golang.org/pkg/strings/)
- [Golang.org Slice-Operationen](https://blog.golang.org/go-slices-usage-and-internals)
- [Go-Tutorial auf Youtube](https://www.youtube.com/watch?v=CF9S4QZuV30&t=36s)