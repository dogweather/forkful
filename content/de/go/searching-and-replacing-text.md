---
title:                "Suchen und Ersetzen von Text"
html_title:           "Go: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum

Wenn du schon einmal einen langen Text geschrieben hast, kennst du sicherlich das Problem von Tippfehlern oder veralteten Begriffen. Das Suchen und Ersetzen von Text kann deine Arbeit erleichtern und Fehler reduzieren.

## Wie es geht

Um Text in Go zu suchen und zu ersetzen, können wir die Funktion `strings.Replace()` verwenden. Diese Funktion erwartet drei Input-Parameter: der zu bearbeitende Text, der zu suchende Text und der neue zu ersetzende Text. Hier ist ein Beispiel, wie du die Funktion verwenden kannst:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    text := "Hallo Welt! Willkommen in der Welt von Go!"
    newText := strings.Replace(text, "Welt", "Golang", 1)
    fmt.Println(newText)
}
```

In diesem Beispiel haben wir den Begriff "Welt" durch "Golang" ersetzt und das Ergebnis in der Variable `newText` gespeichert. Die Ausgabe wird sein: "Hallo Golang! Willkommen in der Welt von Go!" Beachte, dass wir als letzten Parameter `1` übergeben haben, um nur das erste Vorkommen des gesuchten Begriffs zu ersetzen.

Du kannst auch den optionalen 4. Parameter verwenden, um die Anzahl der zu ersetzenden Vorkommen anzugeben. Wenn du `strings.ReplaceAll()` verwendest, wird jeder Vorkommen des gesuchten Begriffs ersetzt.

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    text := "Dies ist ein Text mit vielen Wörtern"
    newText := strings.ReplaceAll(text, "Wörter", "Begriffe")
    fmt.Println(newText)
}
```

Die Ausgabe wird sein: "Dies ist ein Text mit vielen Begriffe".

## Tiefer Einblick

Wenn du genauer verstehen möchtest, wie das Suchen und Ersetzen von Text in Go funktioniert, solltest du dir die Dokumentation zu `strings.Replace()` ansehen. Dort findest du auch Informationen zu anderen Funktionen, die beim Bearbeiten von Strings hilfreich sein können.

## Siehe auch

- [Dokumentation zu `strings.Replace()`](https://pkg.go.dev/strings#Replace)
- [Ein Leitfaden zur effektiven Verwendung von Strings in Go](https://blog.golang.org/strings)