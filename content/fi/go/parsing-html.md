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

# Mitä & Miksi?

HTML-analysointi on prosessi, jossa HTML-muotoista koodia luetaan ja puretaan tietorakenteiksi. Tämä on tärkeä osa web-kehitystä, sillä se mahdollistaa tietojen keräämisen ja manipuloinnin verkkosivuilta. Ohjelmoijat käyttävät tätä työkalua esimerkiksi verkkosivujen datan keräämiseen tai skriptien suorittamiseen automaattisesti.

# Kuinka?

```Go
// Tässä esimerkissä käytetään Go:n sisäänrakennettua HTML-pakettia
package main

import (
    "fmt"
    "strings"
    "code.google.com/p/go.net/html"
)

func main() {
    // Luetaan HTML-koodi merkkijonona
    htmlString := "<html><head><title>Otsikko</title></head><body><h1>Tervetuloa</h1><p>Tämä on esimerkki sivu.</p></body></html>"
    
    // Muutetaan merkkijonosta lukija
    reader := strings.NewReader(htmlString)
    
    // Käydään läpi HTML-koodi ja tulostetaan otsikko ja p-lauseke
    doc, err := html.Parse(reader)
    if err != nil {
        panic(err)
    }
    fmt.Println(doc.FirstChild.FirstChild.LastChild.FirstChild.FirstChild.Data) // Tulostaa: Otsikko
    fmt.Println(doc.LastChild.FirstChild.LastChild.FirstChild.FirstChild.Data) // Tulostaa: Tämä on esimerkki sivu.
}
```

# Deep Dive

HTML-analysoinnilla on pitkät juuret, alkaen ensimmäisten web-sivujen luomisesta 90-luvulla. Tänä päivänä on olemassa muitakin tapoja analysoida HTML-sisältöä, kuten CSS-selektorit ja XPath-kyselyt. Go:n sisäänrakennettu HTML-paketti tarjoaa kuitenkin helpon ja tehokkaan tavan käsitellä HTML-koodia.

# Katso myös

- https://golang.org/pkg/html/ - Go:n HTML-paketti
- https://developer.mozilla.org/en-US/docs/Learn/Getting_started_with_the_web/HTML_basics - Perusteet HTML:stä
- https://www.edureka.co/blog/web-scraping-with-golang/ - Web-scraping Go:lla