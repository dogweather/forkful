---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:00:07.395061-07:00
description: "Hur man g\xF6r: F\xF6r att parsa HTML i Go anv\xE4nder du vanligtvis\
  \ paketet `goquery` eller standardbibliotekets paket `net/html`. H\xE4r \xE4r ett\
  \ grundl\xE4ggande\u2026"
lastmod: '2024-03-13T22:44:37.389653-06:00'
model: gpt-4-0125-preview
summary: "F\xF6r att parsa HTML i Go anv\xE4nder du vanligtvis paketet `goquery` eller\
  \ standardbibliotekets paket `net/html`."
title: Tolka HTML
weight: 43
---

## Hur man gör:
För att parsa HTML i Go använder du vanligtvis paketet `goquery` eller standardbibliotekets paket `net/html`. Här är ett grundläggande exempel som använder `net/html` för att extrahera alla länkar från en webbsida:

```go
package main

import (
    "fmt"
    "golang.org/x/net/html"
    "net/http"
)

func main() {
    // Hämta HTML-dokument
    res, err := http.Get("http://example.com")
    if err != nil {
        panic(err)
    }
    defer res.Body.Close()

    // Tolka HTML-dokumentet
    doc, err := html.Parse(res.Body)
    if err != nil {
        panic(err)
    }

    // Funktion för att rekursivt genomgå DOM
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

    // Genomgå DOM
    f(doc)
}
```

Exempelutdata (med antagandet att `http://example.com` innehåller två länkar):

```
http://www.iana.org/domains/example
http://www.iana.org/domains/reserved
```

Denna kod begär en HTML-sida, tolkar den och genomgår rekursivt DOM för att hitta och skriva ut `href`-attributen för alla `<a>`-taggar.

## Djupdykning
Paketet `net/html` tillhandahåller grunderna för att parsa HTML i Go, genom att direkt implementera tokeniserings- och trädbyggnadsalgoritmerna som specificeras av HTML5-standarden. Detta lågnivåtillvägagångssätt är kraftfullt men kan vara ordrikt för komplexa uppgifter.

I motsats härtil erbjuder det tredjepartspaketet `goquery`, inspirerat av jQuery, ett högre nivå gränssnitt som förenklar DOM-manipulering och genomgång. Det tillåter utvecklare att skriva koncis och uttrycksfull kod för uppgifter som elementval, attributextraktion och innehållsmanipulering.

Dock kommer `goquery`:s bekvämlighet till priset av ett extra beroende och potentiellt långsammare prestanda på grund av dess abstraktionslager. Valet mellan `net/html` och `goquery` (eller andra parsingsbibliotek) beror på projektets specifika krav, såsom behovet av prestandaoptimering eller användarvänlighet.

Historiskt sett har HTML-parsing i Go utvecklats från grundläggande strängoperationer till sofistikerad DOM-trädsmanipulation, vilket återspeglar språkets växande ekosystem och gemenskapens efterfrågan på robusta verktyg för webbskrapning och dataextraktion. Trots inhemska möjligheter, markerar förekomsten av tredjepartsbibliotek som `goquery` Go-gemenskapens preferens för modulariserad, återanvändbar kod. Dock kan programmerare, för prestandakritiska applikationer, fortfarande föredra paketet `net/html` eller till och med ta till regex för enkla parsingsuppgifter, med beaktande av de inneboende riskerna och begränsningarna med regex-baserad HTML-parsing.
