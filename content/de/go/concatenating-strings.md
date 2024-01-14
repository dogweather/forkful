---
title:    "Go: Verkettung von Zeichenfolgen"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/go/concatenating-strings.md"
---

{{< edit_this_page >}}

Warum: Eine kurze Erklärung, warum man sich mit der Verkettung von Zeichenfolgen beschäftigen sollte. 

Wenn Sie Go programmieren, werden Sie früher oder später auf das Konzept der Verkettung von Zeichenfolgen stoßen. Dies ist ein wichtiger Bestandteil der Programmierung, da es Ihnen ermöglicht, verschiedene Texte und Variablen zusammenzufügen, um sinnvolle Ausgaben zu erstellen.

## Wie geht das?

```Go
package main

import "fmt"

func main() {
  hello := "Hallo"
  world := "Welt"
  output := hello + world
  fmt.Println(output)
}
```

Die Ausgabe dieses Codes wird "HalloWelt" sein. Wie Sie sehen können, haben wir die Variablen "Hallo" und "Welt" mithilfe des Plus-Operators zu einer neuen Variablen "output" zusammengefügt. Diese Technik kann auch auf mehrere Variablen angewendet werden, um eine längere Zeichenfolge zu erstellen.

## Tiefer eintauchen

Es gibt noch einige weitere wichtige Dinge zu beachten, wenn Sie Zeichenfolgen in Go verkettet. Zum Beispiel können Sie mit dem Plus-Operator nur Zeichenfolgen des gleichen Typs verknüpfen. Wenn Sie versuchen, einen Integer mit einer Zeichenfolge zu verketten, wird dies zu einem Fehler führen.

Ein weiteres nützliches Werkzeug ist die Funktion "Sprintf", mit der Sie Textformatierungen innerhalb der verknüpften Zeichenfolge vornehmen können. Diese Funktion folgt dem Format "Sprintf("Formatstring", Variablen)", wobei der Formatstring ein Platzhalter für die Variablen ist. 

See Also:

- [Offizielle Dokumentation zu Strings in Go](https://golang.org/pkg/strings/)
- [Eine ausführliche Erklärung zur Zeichenfolgenverkettung in Go](https://www.callicoder.com/golang-strings-cheat-sheet/)
- [Eine Übersicht über die verschiedenen String-Funktionen in Go](https://tutorialedge.net/golang/strings-explained-tutorial/)