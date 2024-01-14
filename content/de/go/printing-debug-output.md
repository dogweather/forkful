---
title:                "Go: Fehlerausgabe drucken"
simple_title:         "Fehlerausgabe drucken"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Debug-Output ist ein unverzichtbares Werkzeug beim Entwickeln von Software. Es ermöglicht uns, den Verlauf und die Ergebnisse unseres Codes zu überprüfen und eventuelle Fehler zu finden. Es hilft uns auch, unsere Gedanken und Entscheidungen während des Entwicklungsprozesses zu dokumentieren.

## Wie man Debug-Output in Go verwendet

Um Debug-Output in Go zu verwenden, müssen wir zunächst die "fmt" Standardbibliothek importieren. Dann können wir die Funktionen "Print" oder "Printf" verwenden, um unsere Ausgaben auf der Konsole anzuzeigen. Hier ist ein einfaches Beispiel:

```Go
package main

import "fmt"

func main() {
  name := "Maria"
  fmt.Print("Hello, ")
  fmt.Printf("%s!", name)
}
```

Die Ausgabe dieses Codes wäre "Hello, Maria!". Wir können auch Variablen und Werte angeben, die wir überwachen möchten, indem wir Platzhalter verwenden. Zum Beispiel:

```Go
package main

import "fmt"

func main() {
  num := 5
  fmt.Printf("The value of num is %d", num)
}
```

Die Ausgabe wäre "The value of num is 5". Durch die Verwendung von Debug-Output können wir überprüfen, ob unsere Variablen die erwarteten Werte haben und somit unsere Annahmen über unser Programm bestätigen oder korrigieren.

## Tiefere Einblicke

Neben "Print" und "Printf" gibt es in Go noch andere nützliche Funktionen für Debug-Output, wie zum Beispiel "Println", "Sprint", "Sprintf" und "Fprintf". Diese Funktionen sind alle in der "fmt" Bibliothek enthalten und haben jeweils leicht unterschiedliche Verwendungszwecke. Es ist wichtig zu beachten, dass der Einsatz dieser Funktionen bei der Performance unserer Anwendung eine Rolle spielen kann, insbesondere wenn sie in Schleifen verwendet werden. Daher sollte Debug-Output nur in Entwicklungsphasen verwendet werden und vor der Veröffentlichung entfernt werden.

## Siehe auch

Hier sind einige Links zu weiteren Informationen über Debug-Output in Go:

- [Offizielle Dokumentation zu fmt](https://golang.org/pkg/fmt/)
- [Ein Artikel über Debugging in Go von The Go Blog](https://blog.golang.org/debugging-go-code-with-panicwrap)
- [Stack Overflow Diskussion über die Verwendung von Debug-Ausgaben in Go](https://stackoverflow.com/questions/12746321/how-to-get-printf-behaviour-in-go)
- [Offizielles GitHub-Repository für das Go-Projekt](https://github.com/golang/go)

Wir hoffen, dass Ihnen dieser Artikel einen guten Überblick darüber gegeben hat, wie Sie Debug-Output in Go verwenden können. Viel Spaß beim Codieren!