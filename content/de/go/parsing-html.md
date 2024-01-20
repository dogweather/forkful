---
title:                "HTML parsen"
html_title:           "Arduino: HTML parsen"
simple_title:         "HTML parsen"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/parsing-html.md"
---

{{< edit_this_page >}}

## Was und Warum?

HTML-Parsing ist der Prozess, HTML-Strings zu analysieren und zu interpretieren. Programmierer tun dies, um Daten aus Webseiten zu extrahieren oder Informationsstruktur der Webseiten zu manipulieren.

## So wird’s gemacht:

Mit dem `net/html` Paket in Go, können wir HTML einfach etablieren. Hier ist ein einfacher Codeausschnitt:

```Go 
package main

import (
  "fmt"
  "golang.org/x/net/html"
  "strings"
)

func main() {
  doc, _ := html.Parse(strings.NewReader("<html><head></head><body>Hello World</body></html>"))
  var f func(*html.Node)
  f = func(n *html.Node) {
    if n.Type == html.TextNode {
      fmt.Println(n.Data)
    }
    for c := n.FirstChild; c != nil; c = c.NextSibling {
      f(c)
    }
  }
  f(doc)
}
```

Wenn du dieses Programm ausführst, wird es "Hello World" auf der Konsole ausgeben.

## Tiefer einsteigen:

HTML-Parsing hat eine lange Geschichte, von Anfang an mit Perl und Regular expressions, bis hin zum aktuellen Stand mit leistungsfähigen Bibliotheken in fast jeder Sprache. Alternativen in Go wären `goquery` zum Beispiel, das eine zusätzliche Schicht über `net/html` schafft und es einfacher macht, bestimmte Knoten zu finden.

Für mehr Kontext darüber, was `net/html` unter der Haube tut: es ist eigentlich eine Basis-Implementierung eines HTML5-Parser gemäß der HTML5-Spezifikation. Es enthält einen Tokenizer, der den HTML String in kleinere Teile bricht, sowie einen Parser, der diese Token dann in eine nachvollziehbare Dokumentenstruktur aufbaut.

## Siehe auch:

- Go net/html Dokumentation: https://pkg.go.dev/golang.org/x/net/html
- Goquery: https://github.com/PuerkitoBio/goquery
- "Web Scraping with Go": Sehr ausführliches Tutorial, auf das sich jeder beziehen kann, der es ernst meint mit Web Scraping in Go: https://edmundmartin.com/web-scraping-with-golang/