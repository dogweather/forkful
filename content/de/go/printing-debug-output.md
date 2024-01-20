---
title:                "Ausgabe von Debugging-Informationen drucken"
html_title:           "Bash: Ausgabe von Debugging-Informationen drucken"
simple_title:         "Ausgabe von Debugging-Informationen drucken"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Was & Warum?

Zum Ausdrucken von Debug-Ausgaben verwendet man oft `fmt.Println()` in Go. Diese Praxis hilft Programmierern, ihre Codes zu prüfen und zu verstehen, wie ihre Anwendungen funktionieren.

## Wie:

Hier ist ein einfacher Codeabschnitt, der eine Debug-Ausgabe druckt:

```Go
package main

import "fmt"

func main() {
    fmt.Println("Dies ist eine Debug-Ausgabe")
}
```

Ausführung diesen Codes würde die folgende Ausgabe ergeben:

```
Dies ist eine Debug-Ausgabe
```

## Vertiefung:

Historischer Kontext: Die Wurzeln der Debug-Ausgabe gehen auf die frühen Tage der Programmierung zurück, als es weniger anspruchsvolle Debugging-Werkzeuge gab. Dennoch ist sie immer noch eine weit verbreitete Praxis, insbesondere für einfache Use-Cases oder schnelle Fehlersuche.

Alternativen: In Go, können Sie auch Pakete wie `log` verwenden, das mehr Flexibilität bietet und zusätzliche Funktionen wie das Protokollieren von Warn- und Fehlermeldungen ermöglicht.

Details zur Implementierung: `fmt.Println()` ist eine Funktion in Go, die die Formatierung und den Druck von Texten vereinfacht. Sie gibt den formatierten Text zusammen mit einem Zeilenumbruch am Ende aus.

## Siehe auch:

- [Go-Dokumentation zu fmt](https://golang.org/pkg/fmt/)
- [Go-Dokumentation zu log](https://golang.org/pkg/log/)
- [Ein kurzer Leitfaden zur Debug-Ausgabe in Go](https://yourbasic.org/golang/fmt-println-print-fprintf/)
- [Eine Anleitung zur Fehlerbehandlung in Go](https://blog.golang.org/error-handling-and-go)