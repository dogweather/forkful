---
title:                "Analyse av HTML"
date:                  2024-01-20T15:31:51.995572-07:00
html_title:           "Arduino: Analyse av HTML"
simple_title:         "Analyse av HTML"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (Hva & Hvorfor?)
Parsing HTML betyr å analysere HTML-kode for å forstå og manipulere dens struktur og innhold. Programmere gjør dette for å hente ut data, migrere innhold eller teste webapplikasjoner.

## How to: (Slik gjør du det:)
```Go
package main

import (
    "fmt"
    "strings"

    "golang.org/x/net/html"
)

func main() {
    // Eksempel HTML
    rawHTML := `<p>Hei, Norge!</p><div><span>Velkommen til Go!</span></div>`

    // Parse HTML
    doc, err := html.Parse(strings.NewReader(rawHTML))
    if err != nil {
        panic(err)
    }

    // Traverser og skriv ut tekstinnhold
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

// Utdata:
// Hei, Norge!
// Velkommen til Go!
```

## Deep Dive (Dypdykk)
Parsing HTML i Go ble enklere med pakken "golang.org/x/net/html", som ble introdusert som del av golangs tredjepartspakker. Før dette var alternativene å bruke regulære uttrykk (ikke anbefalt for HTML) eller andre språk. Detaljene innebærer å bygge et DOM tre og traversere nodene. Man kan finne spesifikke elementer, attributter, og tekstverdier, noe som er essensielt for web scraping og automatisering av oppgaver.

## See Also (Se også)
- Go Docs for "net/html" package: [https://pkg.go.dev/golang.org/x/net/html](https://pkg.go.dev/golang.org/x/net/html)
- Go Query, for jQuery-lignende parsing av HTML i Go: [https://github.com/PuerkitoBio/goquery](https://github.com/PuerkitoBio/goquery)