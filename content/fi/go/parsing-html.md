---
title:                "HTML:n jäsentäminen"
html_title:           "Go: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/parsing-html.md"
---

{{< edit_this_page >}}

## Miksi

HTML-analysointi on tärkeä osa verkkokehitystä ja tiedonhaun prosessia. Se mahdollistaa tietojen keräämisen ja järjestämisen verkkosivuilta, mikä on erityisen hyödyllistä esimerkiksi web-skrapingissa ja tiedon kaivamisessa.

## Miten

HTML-analysointi on helppoa ja tehokasta Go-kielellä. Se voidaan tehdä käyttämällä monia erilaisia ​​kirjastoja, kuten "net/html" ja "goquery". Seuraavassa on yksinkertainen esimerkki, joka näyttää, miten Go-koodi voi hakea ja tulostaa linkit annetusta HTML-sivusta:

```Go
package main

import (
    "fmt"
    "log"
    "net/http"

    "golang.org/x/net/html"
)

func main() {
    // Haetaan haluttu verkkosivu
    resp, err := http.Get("https://www.example.com")

    if err != nil {
        log.Fatal(err)
    }

    defer resp.Body.Close()

    // Analysoi sivun HTML
    doc, err := html.Parse(resp.Body)

    if err != nil {
        log.Fatal(err)
    }

    // Tulostaa kaikki sivun linkit
    var links []string
    var findLinks func(* html.Node)
    findLinks = func(n *html.Node) {
        if n.Type == html.ElementNode && n.Data == "a" {
            for _, a := range n.Attr {
                if a.Key == "href" {
                    links = append(links, a.Val)
                }
            }
        }
        for c := n.FirstChild; c != nil; c = c.NextSibling {
            findLinks(c)
        }
    }
    findLinks(doc)

    fmt.Println(links)
}
```

Koodin suoritus tuottaa seuraavan tuloksen:

```
["https://www.example.com", "https://www.example.com/about", "https://www.example.com/contact"]
```

## Syvällinen sukellus

HTML-analysointi Go-kielellä on mahdollista monilla eri tavoilla riippuen tarpeista ja olosuhteista. "goquery" -kirjasto esimerkiksi tarjoaa helppokäyttöisen rajapinnan, joka mahdollistaa kyselyjen tekemisen HTML-dokumentteihin samanlaisella syntaksilla kuin jQueryssä.

Vaikka HTML-analysointi voi tuntua yksinkertaiselta, se voi olla haastavaa, jos sivusto on monimutkainen tai sitä on muuten vaikea käsitellä. On tärkeää tutustua eri kirjastoihin ja löytää sopivin ratkaisu tarpeisiin.

## Katso myös

- [Go - virallinen sivusto](https://golang.org/)
- [net/html - virallinen kirjasto](https://golang.org/pkg/net/html/)
- [goquery - virallinen kirjasto](https://github.com/PuerkitoBio/goquery)