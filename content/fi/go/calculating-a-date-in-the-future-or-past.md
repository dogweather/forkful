---
title:                "Go: Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi
Monissa ohjelmointiprojekteissa saattaa olla tarve laskea päivämääriä tulevaisuudessa tai menneisyydessä. Tässä blogipostauksessa tarkastelemme miten tämä onnistuu Go-kielen avulla.

## Kuinka tehdä se
```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    // Laskee päivän 30 päivää nykyhetkestä
    tuleva := time.Now().AddDate(0, 0, 30)
    fmt.Println("30 päivän päästä on:", tuleva)

    // Laskee päivän 30 päivää menneisyydestä
    menneisyys := time.Now().AddDate(0, 0, -30)
    fmt.Println("30 päivää sitten oli:", menneisyys)
}
```

Tässä esimerkissä käytämme Go:n `time` pakettia ja sen `AddDate()` funktiota, joka mahdollistaa päivien (ja myös kuukausien ja vuosien) lisäämisen tai vähentämisen nykyhetkestä.

**Tuloste:**

```
30 päivän päästä on: 2019-04-16 12:00:00 +0000 UTC m=+2592000.000000000
30 päivää sitten oli: 2019-02-16 12:00:00 +0000 UTC
```

## Syvemmälle aiheeseen
Go:n `time` paketissa on myös muita hyödyllisiä funktioita, kuten `Date()` jolla voi tarkemmin määrittää päivämäärän ja `Parse()` jolla voi parsia päivämäärän annetusta merkkijonosta. Lisäksi myös aikavyöhyke on mahdollista määrittää Go:n avulla.

## Katso myös
- [Go:n virallinen "time" dokumentaatio] (https://golang.org/pkg/time/)
- [Go:n virallinen opetusvideo päivämäärien laskemisesta] (https://www.youtube.com/watch?v=zQSVhLo1NJA)
- [Esimerkkejä päivämäärien laskemisesta Go-kielellä] (https://github.com/GoesToEleven/GolangTraining/tree/master/47_calender_date_time)