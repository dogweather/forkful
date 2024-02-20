---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:59:57.433822-07:00
description: "HTML parsen in Go houdt in dat je de inhoud van HTML-bestanden analyseert\
  \ om gegevens te extraheren, de structuur te manipuleren of HTML naar andere\u2026"
lastmod: 2024-02-19 22:05:09.369350
model: gpt-4-0125-preview
summary: "HTML parsen in Go houdt in dat je de inhoud van HTML-bestanden analyseert\
  \ om gegevens te extraheren, de structuur te manipuleren of HTML naar andere\u2026"
title: HTML Parsen
---

{{< edit_this_page >}}

## Wat & Waarom?

HTML parsen in Go houdt in dat je de inhoud van HTML-bestanden analyseert om gegevens te extraheren, de structuur te manipuleren of HTML naar andere formaten te converteren. Programmeurs doen dit voor web scraping, templating en datamining, waarbij ze de sterke gelijktijdigheidsmogelijkheden van Go gebruiken voor efficiënte verwerking van grote hoeveelheden webpagina's.

## Hoe te:

Om HTML te parsen in Go, gebruik je typisch het `goquery`-pakket of het standaard `net/html`-pakket uit de bibliotheek. Hier is een basisvoorbeeld waarbij `net/html` wordt gebruikt om alle links van een webpagina te extraheren:

```go
package main

import (
    "fmt"
    "golang.org/x/net/html"
    "net/http"
)

func main() {
    // Haal HTML-document op
    res, err := http.Get("http://example.com")
    if err != nil {
        panic(err)
    }
    defer res.Body.Close()

    // Parse het HTML-document
    doc, err := html.Parse(res.Body)
    if err != nil {
        panic(err)
    }

    // Functie om recursief door de DOM te reizen
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

    // Doorloop de DOM
    f(doc)
}
```

Voorbeeldoutput (ervan uitgaande dat `http://example.com` twee links bevat):

```
http://www.iana.org/domains/example
http://www.iana.org/domains/reserved
```

Deze code vraagt een HTML-pagina op, parset deze en gaat recursief door de DOM om `href` attributen van alle `<a>`-tags te vinden en af te drukken.

## Diepgaande duik

Het `net/html`-pakket biedt de basis voor het parsen van HTML in Go, door direct de tokenisatie- en boomconstructiealgoritmen te implementeren die zijn gespecificeerd door de HTML5-standaard. Deze low-level benadering is krachtig maar kan uitgebreid zijn voor complexe taken.

In tegenstelling, het third-party `goquery`-pakket, geïnspireerd door jQuery, biedt een hoger-niveau interface dat DOM-manipulatie en -traversering vereenvoudigt. Het stelt ontwikkelaars in staat om bondige en expressieve code te schrijven voor taken zoals elementselectie, attribuutextractie en inhoudsmanipulatie.

Echter, het gemak van `goquery` komt met de kosten van een extra afhankelijkheid en potentieel langzamere prestaties vanwege de abstractielaag. De keuze tussen `net/html` en `goquery` (of andere parseerbibliotheken) hangt af van de specifieke vereisten van het project, zoals de behoefte aan prestatieoptimalisatie of gebruiksgemak.

Historisch gezien is het parsen van HTML in Go geëvolueerd van basis string-operaties naar geavanceerde DOM-boommanipulatie, wat de groeiende ecosysteem van de taal en de vraag van de gemeenschap naar robuuste web scraping- en dataverkrijgingstools weerspiegelt. Ondanks de native mogelijkheden, benadrukt de prevalentie van third-party bibliotheken zoals `goquery` de voorkeur van de Go-gemeenschap voor modulaire, herbruikbare code. Echter, voor prestatiekritieke toepassingen kunnen programmeurs nog steeds de voorkeur geven aan het `net/html`-pakket of zelfs terugvallen op regex voor eenvoudige parseertaken, met in gedachten de inherente risico's en beperkingen van op regex gebaseerde HTML-parsing.
