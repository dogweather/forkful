---
title:                "Go: Suchen und Ersetzen von Texten"
simple_title:         "Suchen und Ersetzen von Texten"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum

Wenn Sie jemals versucht haben, einen längeren Text in einem Texteditor zu ändern, wissen Sie, wie zeitaufwändig es sein kann, jede einzelne Stelle zu finden und zu ersetzen. Zum Glück gibt es in Go eine eingebaute Funktion, die es Ihnen ermöglicht, Text einfach und effizient zu suchen und zu ersetzen. In diesem Blogbeitrag werden wir uns ansehen, wie man Text in Go durchsuchen und ersetzen kann.

## Wie funktioniert es?

Um Text in Go zu suchen und zu ersetzen, müssen Sie zuerst das "strings" Paket importieren. Hier ist ein Beispielcode, der zeigt, wie einfach es ist, Text in einem String zu suchen und zu ersetzen:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    s := "Das ist mein Beispieltext."
    replaced := strings.Replace(s, "mein", "dein", 1)
    fmt.Println(replaced)
}
```

Der oben genannte Code würde "Das ist dein Beispieltext." ausgeben. Wie Sie sehen, haben wir die Funktion "Replace" aus dem "strings" Paket verwendet, um den Text "mein" durch "dein" zu ersetzen. Der dritte Parameter "1" gibt an, dass nur die erste Instanz von "mein" ersetzt werden soll. Wenn Sie möchten, dass alle Instanzen ersetzt werden, können Sie einfach "string.Replace(s, "mein", "dein", -1)" verwenden.

## Tiefgehende Informationen

Natürlich können Sie nicht nur einzelne Wörter, sondern auch ganze Sätze oder Zeichenfolgen suchen und ersetzen. Hier ist ein Beispiel dafür, wie Sie alle Vorkommnisse von "ist" in einem Text durch "war" ersetzen können:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    s := "Das ist mein Beispieltext."
    replaced := strings.ReplaceAll(s, "ist", "war")
    fmt.Println(replaced)
}
```

Der Unterschied zu unserem vorherigen Beispiel ist der Einsatz von "ReplaceAll", um alle Vorkommnisse von "ist" zu ersetzen. Sie können auch angeben, dass die Groß- und Kleinschreibung berücksichtigt werden soll, indem Sie "ReplaceAll(strings.ToLower(s), "ist", "war")" verwenden.

Übrigens, muss der Text, in dem Sie suchen und ersetzen, nicht unbedingt eine Variable sein. Sie können auch den Text direkt innerhalb der Funktion übergeben, z.B. "strings.Replace("Das ist mein Beispieltext.", "ist", "war", -1)".

## Siehe auch

- Offizielle Dokumentation zum "strings" Paket: https://golang.org/pkg/strings/
- Eine gute Übersicht über alle in Go verfügbaren String-Funktionen: https://gobyexample.com/string-functions