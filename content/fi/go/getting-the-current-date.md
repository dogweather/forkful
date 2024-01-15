---
title:                "Päivämäärän hakeminen tietokoneella"
html_title:           "Go: Päivämäärän hakeminen tietokoneella"
simple_title:         "Päivämäärän hakeminen tietokoneella"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit saada nykyisen päivämäärän? Käytettyjen ohjelmointikielten joukossaGo on yksi suosituimmista vaihtoehdoista tämän tehtävän suorittamiseen nykypäivänä. Voit käyttää tätä tietoa esimerkiksi sovelluksissa, jotka näyttävät nykyisen päivämäärän tai laskevat ajanjaksojen välistä aikaa.

## Kuinka

Go: n avulla nykyisen päivämäärän saaminen on helppoa. Voit käyttää olemassa olevaa aikapakkausta ja sen tarjoamia toimintoja. Seuraava koodinpätkä näyttää, kuinka voit saada nykyisen päivämäärän ja tulostaa sen konsoliin:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    t := time.Now()  // Haetaan nykyinen päivämäärä
    fmt.Println(t)   // Tulostetaan nykyinen päivämäärä
}
```

Koodin suorittamisen jälkeen saat tulosteen muodossa "2021-09-07 22:57:02.854745 +0300 EEST m=+0.000164655".

## Syventyvä sukellus

Lisäksi aikapaketista löytyy useita muita toimintoja, kuten nykyisen päivän, kuukauden tai vuoden hakeminen erikseen sekä päiväyksen muotoilun muuttaminen. Voit tutustua tarkemmin aikapaketin tarjoamiin toimintoihin ja löytää ne, jotka parhaiten sopivat tarpeisiisi.

## Katso myös

- [Go:n aikapaketin dokumentaatio](https://golang.org/pkg/time/)
- [Lyhyt oppimäärä Go:sta](https://tour.golang.org/welcome/1)