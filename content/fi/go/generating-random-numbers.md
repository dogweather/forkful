---
title:                "Go: Satunnaislukujen luominen"
programming_language: "Go"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi?

Monissa ohjelmointiprojekteissa on tarve käyttää satunnaislukuja, oli kyse sitten pelin pisteiden arpomisesta tai todennäköisyyksien laskemisesta. Go tarjoaa helpon ja tehokkaan tavan generoida satunnaislukuja, mikä tekee siitä suositun vaihtoehdon tässä asiassa.

## Kuinka se tehdään?

Go-kielellä satunnaislukujen luominen on yksinkertaista. Käytämme `rand`-pakettia, joka tarjoaa erilaisia toimintoja satunnaislukujen generoimiseen. Tässä esimerkissä luomme viisi satunnaislukua väliltä 1-100:

```Go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

func main() {
    // Asetetaan generoijan siemeneksi nykyisen ajan sekunnit
    rand.Seed(time.Now().Unix())

    // Käytetään for-silmukkaa luomaan viisi satunnaislukua
    for i := 0; i < 5; i++ {
        // Kutsutaan rand.Intn() funktiota, joka palauttaa satunnaisen luvun väliltä 0-99
        // Lisätään tähän 1, jotta saamme lukujen väliltä 1-100
        fmt.Println(rand.Intn(100) + 1)
    }
}
```

Esimerkin tulostus voisi näyttää tältä:

```
81
12
45
23
97
```

Kuten näemme, saamme joka kerta ohjelman suorittaessa uuden satunnaisen luvun. Toistan: `rand.Seed()`-funktiolla asetetaan generoijan siemeneksi nykyisen ajan sekunnit, jotta saamme joka kerta erilaisen satunnaislukusarjan. Tämän vuoksi on tärkeää asettaa siemen ennen satunnaislukujen luomista.

## Syvemmältä satunnaislukujen luomiseen

Jos haluat syvällisempää tietoa satunnaislukujen generoinnista Go-kielellä, voit tutustua Go:n `math/rand`-pakettiin ja sen tarjoamiin erilaisiin toimintoihin. Voit myös käyttää `time`-pakettia erilaisten siementen asettamiseen. Kannattaa myös tutustua pseudoruuhuisiin, jotka ovat algoritmeja satunnaisluvun luomiseen.

## Katso myös

- [Go:n rand-paketin dokumentaatio](https://golang.org/pkg/math/rand/)
- [Go:n time-paketin dokumentaatio](https://golang.org/pkg/time/)
- [Pseudoruhien vertailu Go:ssa](https://github.com/dgryski/go-pcg)