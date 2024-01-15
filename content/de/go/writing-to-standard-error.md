---
title:                "Schreiben auf Standardfehler"
html_title:           "Go: Schreiben auf Standardfehler"
simple_title:         "Schreiben auf Standardfehler"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte jemand seine Fehlermeldungen in Go-Code an den Standardfehlerausgabestrom schreiben? Nun, das Schreiben an den Standardfehlerausgabestrom ist oft nützlich, um Fehler in der Ausführung eines Programms zu identifizieren, da sie direkt auf dem Bildschirm angezeigt werden und somit schneller erkannt werden können.

## Wie geht das?

Es ist ganz einfach, Fehler an den Standardfehlerausgabestrom in Go zu schreiben. Alles, was du tun musst, ist die "fmt" Bibliothek zu importieren und die Funktion "Fprintf" zu verwenden, die es dir ermöglicht, direkt in den Standardfehlerstrom zu schreiben.

```Go
  import "fmt"

  func main() {
    fmt.Fprintf(os.Stderr, "Dies ist eine Fehlermeldung")
  }
```

Die oben genannten Codezeilen werden eine einfache Fehlermeldung auf dem Bildschirm ausgeben. Du kannst jedoch auch zusätzliche Informationen wie Datei- oder Zeilennummern angeben, indem du die Printf-Funktion verwendest und Platzhalter in deinen Fehlermeldungen verwendest.

```Go
  import "fmt"

  func main() {
    fmt.Fprintf(os.Stderr, "Fehler in Datei %s auf Zeile %d", filename, lineNum)
  }
```

## Tiefer Einblick

Um einen tieferen Einblick in das Schreiben von Fehlern an den Standardfehlerausgabestrom in Go zu bekommen, solltest du dir die verschiedenen Möglichkeiten ansehen, wie du die Printf-Funktion verwenden kannst. Du kannst zum Beispiel Farbcodes verwenden, um deine Fehlermeldungen hervorzuheben oder benutzerdefinierte Funktionen schreiben, um spezifische Typen von Fehlern zu behandeln. Es gibt unzählige Möglichkeiten, um deine Fehlermeldungen effektiver zu gestalten und somit das Debugging deines Codes zu erleichtern.

## Siehe auch

- [Go Standardbibliothek "fmt"](https://golang.org/pkg/fmt/)
- [Effektives Debugging in Go](https://medium.com/@nazmulnyc/effective-debugging-in-go-a4bcc59a05a1)
- [Anzeige von Farben in der Konsolenausgabe mit Go](https://medium.com/swlh/displaying-colors-in-console-output-with-go-e339c07b0c4f)