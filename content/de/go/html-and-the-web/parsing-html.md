---
title:                "HTML parsen"
aliases:
- /de/go/parsing-html/
date:                  2024-02-03T17:59:51.454246-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML parsen"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Das Parsen von HTML in Go umfasst die Analyse des Inhalts von HTML-Dateien, um Daten zu extrahieren, die Struktur zu manipulieren oder HTML in andere Formate zu konvertieren. Programmierer tun dies für Web Scraping, Templating und Data Mining, wobei sie die starken Concurrency-Features von Go für die effiziente Verarbeitung großer Mengen von Webseiten nutzen.

## Wie:

Um HTML in Go zu parsen, verwendet man üblicherweise das `goquery`-Paket oder das Standardbibliothekspaket `net/html`. Hier ist ein einfaches Beispiel, das `net/html` verwendet, um alle Links von einer Webseite zu extrahieren:

```go
package main

import (
    "fmt"
    "golang.org/x/net/html"
    "net/http"
)

func main() {
    // HTML-Dokument abrufen
    res, err := http.Get("http://example.com")
    if err != nil {
        panic(err)
    }
    defer res.Body.Close()

    // Das HTML-Dokument parsen
    doc, err := html.Parse(res.Body)
    if err != nil {
        panic(err)
    }

    // Funktion zur rekursiven Durchquerung des DOM
    var f func(*html.Node)
    f = func(n *html.Node) {
        if n.Type == html.ElementNode && n.Data == "a" {
            for _, a := range n.Attr {
                if a.Key == "href" {
                    fmt.Println(a.Val)
                    break
                }
            }
        }
        for c := n.FirstChild; c != nil; c = c.NextSibling {
            f(c)
        }
    }

    // Den DOM durchqueren
    f(doc)
}
```

Beispielausgabe (angenommen, `http://example.com` enthält zwei Links):

```
http://www.iana.org/domains/example
http://www.iana.org/domains/reserved
```

Dieser Code fordert eine HTML-Seite an, parst sie und durchläuft rekursiv den DOM, um `href`-Attribute aller `<a>`-Tags zu finden und auszudrucken.

## Vertiefung

Das `net/html`-Paket bietet die Grundlagen für das Parsen von HTML in Go und implementiert direkt die Tokenisierungs- und Baumkonstruktionsalgorithmen, die durch den HTML5-Standard spezifiziert sind. Dieser low-level Ansatz ist leistungsfähig, kann aber für komplexe Aufgaben umfangreich sein.

Im Gegensatz dazu bietet das Drittanbieterpaket `goquery`, inspiriert von jQuery, eine höherstufige Schnittstelle, die DOM-Manipulation und -Durchquerung vereinfacht. Es ermöglicht Entwicklern, prägnanten und ausdrucksstarken Code für Aufgaben wie Elementauswahl, Attributextraktion und Inhaltsmanipulation zu schreiben.

Die Bequemlichkeit von `goquery` geht jedoch auf Kosten einer zusätzlichen Abhängigkeit und potenziell langsamerer Leistung durch seine Abstraktionsschicht. Die Wahl zwischen `net/html` und `goquery` (oder anderen Parsingsbibliotheken) hängt von den spezifischen Anforderungen des Projekts ab, wie beispielsweise dem Bedarf an Leistungsoptimierung oder Benutzerfreundlichkeit.

Historisch gesehen hat sich das HTML-Parsing in Go von einfachen Stringoperationen zu ausgeklügelten DOM-Baummanipulationen entwickelt, was das wachsende Ökosystem der Sprache und die Nachfrage der Community nach robusten Werkzeugen für Web Scraping und Datengewinnung widerspiegelt. Trotz der nativen Fähigkeiten zeigt die Verbreitung von Drittanbieterbibliotheken wie `goquery` die Präferenz der Go-Community für modularen, wiederverwendbaren Code. Für leistungsorientierte Anwendungen bevorzugen Programmierer jedoch möglicherweise immer noch das `net/html`-Paket oder greifen sogar für einfache Parsingaufgaben auf Regex zurück, wobei sie die inhärenten Risiken und Beschränkungen des auf Regex basierenden HTML-Parsings im Auge behalten.
