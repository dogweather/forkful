---
title:                "Kahden päivämäärän vertaaminen"
html_title:           "Bash: Kahden päivämäärän vertaaminen"
simple_title:         "Kahden päivämäärän vertaaminen"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Päivämäärien vertaaminen on ohjelmoijan taito, jossa kaksi tai useampia päivämääriä verrataan keskenään. Päivämäärävertailuja käytetään esimerkiksi saamaan selville, mikä päivämäärä tulee ensin tai kuinka monta päivää on kahden päivämäärän välillä.

## Näin Se Tehdään:

Go-kielisessä ohjelmoinnissa voit tehdä päivämäärävertailuja ajan paketin (`time package`) avulla. Esimerkki:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    pvm1 := time.Date(2022, 1, 1, 0, 0, 0, 0, time.UTC)
    pvm2 := time.Date(2023, 1, 1, 0, 0, 0, 0, time.UTC)

    if pvm1.Before(pvm2) {
        fmt.Println("Päivämäärä1 tulee ennen Päivämäärää2")
    } else {
        fmt.Println("Päivämäärä2 tulee ennen Päivämäärää1")
    }
}
```

Ohjelma tulostaa:
```
Päivämäärä1 tulee ennen Päivämäärää2
```

## Syvemmälle

Ensinnäkin, on hyvä mainita, että Go-ohjelmointikieli lanseerattiin vuonna 2007, ja sen perustajat suunnittelivat sen siten, että se olisi helppo oppia ja käyttää. Tästä syystä päivämäärien vertaaminen Go:ssa on yksinkertaista.

Toiseksi, on olemassa vaihtoehtoinen tapa vertailla päivämääriä Go:ssa. Voit vertailla kahta päivämäärää käyttäen `After` tai `Equal` menetelmiä.

Viimeiseksi, Go:n ajan (`time`) paketti antaa monia työkaluja päivämäärän ja ajan käsittelyyn. Päivämäärien vertailussa käytetyt `Before`, `After` ja `Equal` -metodit ilmoittavat, tuleeko tietty päivämäärä ennen, jälkeen tai onko se sama kuin toinen päivämäärä.

## Katso Myös

[Go:n virallinen ajan paketin dokumentaatio](https://golang.org/pkg/time/)
[Go:n päivämäärän ja ajan käsittelyn opas](https://yourbasic.org/golang/format-parse-string-time-date-example/)