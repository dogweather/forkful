---
title:                "Go: HTML-Analyse"
simple_title:         "HTML-Analyse"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/parsing-html.md"
---

{{< edit_this_page >}}

## Warum

Das Parsen von HTML kann für EntwicklerInnen in Go sehr nützlich sein, um Daten von Websites oder APIs zu extrahieren und weiterzuverarbeiten.

## Wie man es macht

Das Parsen von HTML in Go ist relativ einfach mit der Verwendung der `goquery` Bibliothek. Hier ist ein Beispiel, um alle Links auf einer Webseite zu extrahieren:

```Go
package main

import (
  "fmt"
  "log"

  "github.com/PuerkitoBio/goquery"
)

func main() {
  // Webseite laden
  doc, err := goquery.NewDocument("https://www.example.com")
  if err != nil {
    log.Fatal(err)
  }

  // Alle Links auf der Seite sammeln
  doc.Find("a").Each(func(index int, element *goquery.Selection) {
    link, _ := element.Attr("href")
    fmt.Printf("Link #%d: %s\n", index, link)
  })
}
```

Die Ausgabe sieht dann in etwa so aus:

```
Link #0: https://www.example.com/first
Link #1: https://www.example.com/second
Link #2: https://www.example.com/third
...
```

## Tiefergehende Informationen

Das Parsen von HTML in Go ermöglicht es EntwicklerInnen, gezielt nach bestimmten Elementen oder Klassen zu suchen und diese zu extrahieren. Mit der `Selection` Struktur von goquery können verschiedene Methoden wie `Find()` und `Text()` verwendet werden, um die gewünschten Daten auszuwählen und zu formatieren.

Ein weiterer wichtiger Aspekt beim Parsen von HTML ist die Verwendung von Selectors, um spezifischere Elemente auszuwählen. Mit der Unterstützung von CSS-Selektoren in goquery können EntwicklerInnen ihre Suchanfragen noch genauer definieren.

## Siehe auch

- [Die offizielle Dokumentation von goquery](https://godoc.org/github.com/PuerkitoBio/goquery)
- [Ein Tutorial zum Parsen von HTML in Go](https://www.youtube.com/watch?v=RW4vv8G9Ubo)
- [Weitere Beispiele und Anwendungsgebiete für das Parsing von HTML in Go](https://github.com/avelino/awesome-go#html)