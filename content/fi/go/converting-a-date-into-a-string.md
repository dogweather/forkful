---
title:    "Go: Muunna päivämäärä merkkijonoksi"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Miksi
Monissa ohjelmointiprojekteissa saattaa olla tarve muuntaa päivämäärä merkkijonoksi. Tässä blogikirjoituksessa tarkastelemme miten tämä voidaan tehdä Go-ohjelmointikielellä.

## Miten
Go-ohjelmointikielessä on olemassa valmiita funktioita päivämäärän muuntamiseen merkkijonoksi. Käytetään esimerkiksi aikaa 17.10.2021.

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    t := time.Date(2021, time.October, 17, 0, 0, 0, 0, time.UTC)
    // t muutetaan merkkijonoksi halutun muodon mukaan
    s := t.Format("02.01.2006")
    fmt.Println(s) // Tulostaa 17.10.2021
    // Voidaan myös käyttää muita muotoja, esimerkiksi:
    s2 := t.Format("Mon. Jan 2 2006")
    fmt.Println(s2) // Tulostaa Sun. Oct 17 2021
}
```

## Syvemmälle
Go:ssa päivämäärän muuntaminen merkkijonoksi perustuu "layout"-merkkijonoon, jossa määritellään haluttu muoto. Esimerkiksi "02.01.2006" vastaa muotoa päivä.kuukausi.vuosi. Voit tarkastella kaikkia mahdollisia "layout"-merkkijonoja Go:n aikapakettiin dokumentoinnista. On myös mahdollista määrittää omia päivämäärän muotoja.

## Katso myös
- [Go:n aikapaketin dokumentaatio](https://pkg.go.dev/time)
- [Go:n aikapaketin esimerkkejä](https://golang.org/pkg/time/#pkg-examples)