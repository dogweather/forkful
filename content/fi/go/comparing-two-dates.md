---
title:                "Go: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Miksi vertailla kahta päivämäärää Go-koodilla?

Päivämäärien vertailu on yleinen tarve lähes kaikissa ohjelmointiprojekteissa. Oli kyse sitten tapahtumien järjestämisestä tai ajan laskemisesta, päivämäärien vertaaminen on välttämätöntä. Go-ohjelmointikielen avulla tämä tehtävä on helppo toteuttaa, ja tässä blogikirjoituksessa selvitämme miten.

## Kuinka vertailla kahta päivämäärää Go-koodilla?

Päivämäärien vertaaminen Go-koodilla onnistuu helposti time-paketin avulla. Alla olevassa esimerkissä luomme kaksi päivämäärää ja vertailemme niitä käyttäen before ja after tarkistusmuuttujia.

```Go
package main

import (
    "fmt"
    "time"
)

func main() {

    pvm1 := time.Date(2021, time.January, 1, 0, 0, 0, 0, time.UTC)
    pvm2 := time.Date(2021, time.February, 1, 0, 0, 0, 0, time.UTC)

    before := pvm1.Before(pvm2)
    after := pvm1.After(pvm2)

    fmt.Println("Päivämäärä 1 ennen päivämäärää 2:", before)
    fmt.Println("Päivämäärä 1 jälkeen päivämäärää 2:", after)
}
```
Tämä koodi tulostaa seuraavan:

Päivämäärä 1 ennen päivämäärää 2: true
Päivämäärä 1 jälkeen päivämäärää 2: false

Go:ssa päivämäärät ovat time.Time -tyyppisiä objekteja, joiden avulla voidaan suorittaa erilaisia toimintoja kuten vertailua. Päivämäärien tarkkuus on nanosekunteihin asti, mikä tekee Go:sta tehokkaan työkalun päivämäärien käsittelyyn.

## Syvällinen tarkastelu

Päivämäärien vertailu Go-koodilla perustuu ajan hetkien väliseen vertailuun. Käyttämällä before ja after metodeja voidaan selvittää onko jokin ajan hetki ennen vai jälkeen toista.

Toinen tapa vertailla päivämääriä on käyttää Equal-metodia, joka kertoo onko kaksi päivämäärää samat. Tällä tavoin voidaan tarkistaa esimerkiksi onko kaksi tapahtumaa tapahtunut samana päivänä.

Go:ssa on myös mahdollista tehdä aikojen välistä laskentaa, kuten laskea kuinka monta päivää tai tuntia on kahden päivämäärän välillä. Tämä tapahtuu käyttämällä Sub-metodia, joka vähentää kahdesta päivämäärästä toisen toisesta.

## Katso myös

- [Time-paketin dokumentaatio](https://golang.org/pkg/time/)
- [Go-opas päivämäärien käsittelyyn](https://golangbot.com/dates-in-go/)

Kiitos kun luit tämän blogikirjoituksen! Toivottavasti se auttoi sinua ymmärtämään päivämäärien vertailun perusteet Go-koodilla. Muista kuitenkin, että on aina hyvä tutustua kunkin projektin tarpeisiin räätälöityihin toimintoihin ja metodeihin. Onnea ohjelmointiin!