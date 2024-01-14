---
title:                "Go: Kahden päivämäärän vertailu"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmoinnin projekteissa on tarve vertailla eri päivämääriä, esimerkiksi tarkistaakseen onko jokin tapahtuma jo mennyt. Tässä blogikirjoituksessa käymme läpi, miten tämä voidaan toteuttaa Go-ohjelmointikielellä.

## Kuinka

Yksi tapa vertailla päivämääriä on käyttää aikaleimoja (timestamps), jotka ovat kokonaislukuja, jotka esittävät ajanhetkeä tiettynä sekuntina. Go tarjoaa time-paketin, jossa on mahdollista luoda aikaleimoja ja vertailla niitä. Alla on yksinkertainen esimerkki, jossa verrataan kahta päivämäärää ja tulostetaan tulos konsoliin:

```Go
package main
import (
    "fmt"
    "time"
)
func main() {
    date1 := time.Date(2021, time.May, 10, 12, 0, 0, 0, time.UTC)
    date2 := time.Date(2021, time.May, 11, 12, 0, 0, 0, time.UTC)
    
    if date1.Before(date2) {
        fmt.Println("Päivämäärä 1 on ennen päivämäärää 2")
    } else {
        fmt.Println("Päivämäärä 2 on ennen päivämäärää 1")
    }
}
```

Tulostus:

```
Päivämäärä 1 on ennen päivämäärää 2
```


## Syvällisemmin

Go-ohjelmointikielessä aikaleimoja käsitellään time.Time-tyypin avulla. Tämä tyyppi sisältää kaikki tarvittavat ominaisuudet ja metodit päivämäärien vertailuun. Aikaleimoja voidaan myös muuttaa eri aikavyöhykkeiden välillä, mikä helpottaa kansainvälisten sovellusten kehitystä.

On myös tärkeää huomata, ettei päivämääriä ja aikaleimoja tulisi koskaan vertailla ==- tai !=-operaattoreilla. Aikaleimojen vertailuun tulisi aina käyttää Before(), After() tai Equal() -metodeja, jotka tarkastavat myös mahdolliset aikavyöhykkeiden erot.


## Katso myös

- [Go time-paketti](https://golang.org/pkg/time/)
- [Vertaile aikaleimoja Go:ssa](https://medium.com/@saturnsrings/time-manipulation-based-on-millisecond-data-in-golang-3bcace3aad7d)