---
title:                "Go: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/parsing-html.md"
---

{{< edit_this_page >}}

## Miksi

Oletko koskaan ihmetellyt, miten verkkosivujen sisällön tiedot saadaan muodostettua ja näytettyä meille? Tämä on mahdollista HTML-parsingin avulla, joka käsittelee verkkosivujen koodia ja muuttaa sen lukukelpoiseen muotoon. Lue tämä blogikirjoitus, niin opit lisää siitä, miksi HTML-parsing on tärkeää ja miten se voidaan toteuttaa Go-ohjelmointikielellä.

## Miten

```Go
package main

import (
    "fmt"
    "net/http"
    "golang.org/x/net/html"
)

func main() {
    // Alustetaan HTTP-pyynnöstä saatu verkkosivu muuttujaan
    response, err := http.Get("https://example.com")

    // Tarkistetaan mahdolliset virheet
    if err != nil {
        fmt.Println(err)
    }

    // Luodaan HTML-tiedoston muodostamiseen tarvittava "token"-muuttuja
    token := html.NewTokenizer(response.Body)

    // Käydään läpi sivun "tokenit"
    for {
        t := token.Next()

        // Kun token on "Element"-tyyppiä
        if t == html.ErrorToken {
            // Poistutaan funktiosta
            return
        }

        // Tarkistetaan onko lohkon alku "text/html"-merkinnällä
        if token.Token().Data == "text/html" {
            // Tulostetaan lohkon sisältö
            fmt.Println(token.Token().Data)
        }
    }
}
```

Tässä esimerkissä käytetään Go-kielen "net/http" ja "golang.org/x/net/html" kirjastoja HTML-parsingiin. Koodi lataa halutun verkkosivun ja tulostaa sen koodin lukukelpoiseen muotoon. Tämän avulla voimme käsitellä verkkosivujen sisältöä ja etsiä esimerkiksi tiettyjä elementtejä tai tietoja sivulta.

Esimerkissä käytetään myös for-loopia, jolla käydään läpi verkkosivulta saadut "token"-tiedot ja tarkistetaan niiden sisältö. Tämä on hyvä tapa aloittaa HTML-parsingin toteuttaminen Go-kielellä.

## Syvällinen sukellus

HTML-parsing on tärkeää erityisesti silloin, kun haluamme käsitellä verkkosivujen sisältöä joko itse tai jonkin sovelluksen avulla. Se mahdollistaa tiedon muokkaamisen ja purkamisen helpommin ja tehokkaammin kuin suoraan lukemalla verkkosivun koodia.

Go-kielellä on myös monia muita kirjastoja, jotka mahdollistavat edistyksellisemmän HTML-parsingin, kuten goquery ja colly. Nämä kirjastot tarjoavat enemmän toiminnallisuuksia ja käyttömahdollisuuksia, kun halutaan syvällisempää tai monimutkaisempaa HTML-parsingia.

## Katso myös

- [Go:n "net/http" kirjasto](https://golang.org/pkg/net/http/)
- [Go:n "golang.org/x/net/html" kirjasto](https://golang.org/x/net/html/)
- [Goquery-kirjasto](https://github.com/PuerkitoBio/goquery)
- [Colly-kirjasto](https://github.com/gocolly/colly)