---
title:                "HTML Parsen"
date:                  2024-01-28T22:03:40.671443-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML Parsen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/parsing-html.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
HTML parsen betekent informatie extraheren uit een HTML-bestand – dat is de code achter webpagina's. Programmeurs doen dit om gegevensophaling te automatiseren, inhoud te extraheren en inhoud tussen systemen te migreren.

## Hoe:
Go heeft een `net/html` pakket perfect voor het duiken in HTML. Hier is de essentie:

```Go
package main

import (
	"fmt"
	"golang.org/x/net/html"
	"net/http"
	"os"
)

func main() {
	// HTML ophalen
	resp, err := http.Get("http://example.com")
	if err != nil {
		fmt.Fprintf(os.Stderr, "ophalen: %v\n", err)
		os.Exit(1)
	}
	defer resp.Body.Close()
	
	// HTML parsen
	doc, err := html.Parse(resp.Body)
	if err != nil {
		fmt.Fprintf(os.Stderr, "parsen: %v\n", err)
		os.Exit(1)
	}

	// De HTML nodenboom doorlopen
	var f func(*html.Node)
	f = func(n *html.Node) {
		if n.Type == html.ElementNode && n.Data == "a" {
			for _, a := range n.Attr {
				if a.Key == "href" {
					fmt.Printf("%v\n", a.Val)
				}
			}
		}
		for c := n.FirstChild; c != nil; c = c.NextSibling {
			f(c)
		}
	}
	f(doc)
}
```

Draai het? Je krijgt links van `example.com`, afgedrukt op je console. Makkelijk!

## Diepgaande duik:
Hier is het verhaal. Toen het web nieuw was, was HTML eenvoudig. Niet meer. Vandaag is het complex, vol met nuances.

Waarom geen regex? HTML kan inconsistent zijn. Regex voor HTML is een onbetrouwbare, foutgevoelige benadering. Parsers zoals `net/html` zijn slimmer. Ze handelen eigenaardigheden en nestings in HTML af die een regex-patroon zou breken.

De `net/html` parser bouwt een boom van HTML-elementen. Het geeft structuur aan een warboel van takken - chaos omzetten in iets waar je doorheen kunt klimmen. Je doorloopt de boom met je eigen functies om tags en attributen te zeven.

Wat zou je nog meer kunnen gebruiken? Bibliotheken zoals `goquery` bieden een jQuery-achtige ervaring voor Go, en `colly` is een populaire keuze voor scraping.

## Zie ook:
- Go's `net/html` pakket: https://pkg.go.dev/golang.org/x/net/html
- GoQuery voor een jQuery-achtige syntax: https://github.com/PuerkitoBio/goquery
- Colly voor scraping: http://go-colly.org/
