---
title:                "Go: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi harrastaa nykyisen päivämäärän hakemista?

Go-ohjelmointi on noussut suureen suosioon viime vuosina sen yksinkertaisuuden ja tehokkuuden vuoksi. Yksi yleinen tehtävä, johon usein tarvitaan ohjelmointitaidot, on nykyisen päivämäärän hakeminen. Tämä voidaan tehdä helposti Go-kielellä ja tässä blogikirjoituksessa tarkastelemme tätä prosessia.

## Kuinka tehdä se?

Nykyisen päivämäärän hakeminen voidaan suorittaa muutamalla rivillä Go-koodia. Käytämme "time" pakettia, joka sisältää kaikki tarvittavat toiminnot päivämäärän ja ajan hallintaan. Seuraavassa esimerkissä näet, kuinka voit helposti saada nykyisen päivämäärän ja tulostaa sen terminaaliin:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    // Haetaan nykyinen päivämäärä
    nykyinenPaivamaara := time.Now()

    // Muotoillaan halutun muotoon
    muotoiltuPaivamaara := nykyinenPaivamaara.Format("02.01.2006")

    // Tulostetaan terminaaliin
    fmt.Println(muotoiltuPaivamaara)
}
```

Tämä koodi tulostaisi terminaaliin nykyisen päivämäärän muodossa "10.08.2021". Huomaa, että päivämäärän muotoilu tapahtuu muutamalla numerolla (02.01.2006), mikä voi olla hieman hämmentävää aluksi. Voit kuitenkin muokata muotoa halutulla tavalla kopioimalla ja liittämällä eri numerot. Esimerkiksi jos haluat tulostaa päivämäärän muodossa "08/10/2021", muotoilu olisi "01/02/2006".

## Syvällisempää tietoa

Nykyisen päivämäärän hankkiminen voi tuntua yksinkertaiselta tehtävältä, mutta Go-kielellä on paljon mahdollisuuksia ainutlaatuiseen muotoiluun ja mukauttamiseen. Käyttämällä "time" pakettia, voit myös hankkia tietoa, kuten kuukauden nimen tai viikonpäivän nimen. Voit jopa määrittää oman aikavyöhykkeesi, jos haluat saada päivämäärän eri aikavyöhykkeellä.

## Katso myös

- [Ajan ja päivämäärän hallinta Go-kielellä](https://gobyexample.com/time)
- [Virallinen "time" paketin dokumentaatio](https://golang.org/pkg/time/)
- [Tutustu Go-kielen perusteisiin](https://go.dev/learn/)