---
title:                "HTML parsen"
date:                  2024-01-20T15:31:58.087971-07:00
html_title:           "Arduino: HTML parsen"
simple_title:         "HTML parsen"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/parsing-html.md"
---

{{< edit_this_page >}}

## Was & Warum?
HTML-Parsing ermöglicht es, den Inhalt und die Struktur von Webseiten zu analysieren und Daten daraus zu extrahieren. Programmierer nutzen es, um auf relevante Informationen gezielt zuzugreifen und automatisiert zu verarbeiten.

## Wie geht das?
```Go
package main

import (
	"fmt"
	"golang.org/x/net/html"
	"net/http"
)

func main() {
	resp, err := http.Get("http://beispiel.de")
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()

	doc, err := html.Parse(resp.Body)
	if err != nil {
		panic(err)
	}

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

	f(doc)
}
```
Beispiel Ausgabe:
```
http://example.com/link1
http://example.com/link2
```

## Tiefen Tauchgang
HTML-Parsing ist seit den Anfängen des Webs entscheidend. Die Go-Standardbibliothek bietet grundlegende Werkzeuge, wie das `net/html` Paket. Alternativen wie `goquery` imitieren jQuery für einen einfacheren Umgang mit dem DOM. Beim Parsen von HTML kommt es darauf an, korrekt auf das sich ständig ändernde Mark-up von Webseiten zu reagieren und fehlertolerant zu sein.

## Siehe Auch
- Go-Dokumentation für das `net/html` Paket: https://pkg.go.dev/golang.org/x/net/html
- goquery-Projektseite: https://github.com/PuerkitoBio/goquery
- Einführung in das Parsen von HTML mit Go: https://schier.co/blog/2015/04/26/a-simple-web-scraper-in-go.html
