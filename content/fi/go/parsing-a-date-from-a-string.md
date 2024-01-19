---
title:                "Päivämäärän jäsentäminen merkkijonosta"
html_title:           "Bash: Päivämäärän jäsentäminen merkkijonosta"
simple_title:         "Päivämäärän jäsentäminen merkkijonosta"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Päivämäärän jäsennys merkkijonosta Golla

## Mikä & Miksi?
Päivämäärän jäsentyminen merkkijonosta on prosessi, jossa merkkijono muutetaan päivämääräksi tai aikaleimaksi. Ohjelmoijat tekevät tämän, jotta he voivat suorittaa päivämäärään ja aikaan perustuvia toimintoja, kuten ajastettuja tapahtumia.

## Näin se tehdään:
Go-ohjelmointikielessä voit jäsentää päivämäärän `time.Parse` -toiminnolla. Tässä on esimerkki:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	fmt.Println("Tänään " + time.Now().Format("02-01-2006"))
	t, _ := time.Parse("02-01-2006", "20-12-2022")
	fmt.Println("Joulun ajankohta: ", t)
}
```

Kun suoritat yllä olevan koodin, tuotos näyttää seuraavasti:

```Go
Tänään 13-03-2022
Joulun ajankohta:  2022-12-20 00:00:00 +0000 UTC
```

## Syvällinen tarkastelu
Päivämäärän jäsennys merkkijonosta on ollut olennainen osa ohjelmointia jo vuosikymmeniä. Go tarjoaa joustavan `time`-paketin, jonka avulla päivämäärän jäsennys on helppoa ja suoraviivaista.

Vaihtoehdoiksi `time.Parse`:lle löytyy lukuisia kirjastoja, kuten `go-date`, joka tarjoaa lisäominaisuuksia päivämäärien käsittelyyn. Kuitenkin, useimmissa tapauksissa, käyttökelpoisin vaihtoehtojen joukossa on sisäänrakennettu `time`-paketti.

Go:n `time.Parse` -funktio käyttää erityisiä layout-merkkijonoja, jotka määrittelevät päivämäärän ja ajan formaatin. Tämä layout on peräisin Go:n ajan nopeudesta (joka on tarkalleen nähtävillä `time`-paketin dokumentaatiossa) ja sitä käytetään mallina syötteen jäsentämisessä.

## Katso myös
- Go:n virallinen time paketin dokumentaatio: http://golang.org/pkg/time/
- Erinomainen artikkeli, joka käsittelee Go:n date-tietueen jäsentämistä: https://gobyexample.com/time-formatting-parsing
- Open-source Go päivämääräkirjasto lisäominaisuuksia varten: https://github.com/jinzhu/now