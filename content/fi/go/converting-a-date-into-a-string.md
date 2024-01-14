---
title:                "Go: Päivämäärän muuntaminen merkkijonoksi"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Monissa Go-ohjelmoinnin projekteissa saattaa joskus olla tarve muuttaa päivämäärä muotoon, joka on ymmärrettävämpi käyttäjälle tai tallennusta varten. Tässä blogikirjoituksessa tutustutaan kuinka tähän tehtävään voidaan lähteä käsiksi kannustamalla lähdekoodeihin ja ketteriin ohjelmistotyökaluihin.

## Kuinka tehdä

Katsotaanpa ensin kuinka muuttaa päivämäärä päivämäärärajapinnan avulla. Voimme käyttää aikapakettia luodaksemme haluamamme ajanhetken ja käyttää sitten MarshalJSON-menetelmää muuttaaksemme sen merkkijonoksi. Tässä on yksinkertainen esimerkki:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    t := time.Date(2021, time.January, 1, 0, 0, 0, 0, time.UTC)
    json, err := t.MarshalJSON()
    if err != nil {
        fmt.Println(err)
    }
    fmt.Println(string(json))
}
```

Tulostus:

```
"2021-01-01T00:00:00Z"
```

Voit myös muuttaa päivämäärän haluttuun muotoon käyttämällä aika- ja merkkijonopaketteja. Tässä on esimerkki tähän lähestymistapaan:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    t := time.Date(2020, time.October, 10, 12, 0, 0, 0, time.UTC)
    layout := "1-2-2006"
    str := t.Format(layout)
    fmt.Println(str)
}
```

Tulostus:

```
10-10-2020
```

## Syvenny

Syvemmälle mentäessä voimme huomata, että päivämäärämuunnos Go-kielessä on erittäin joustavaa. Aikapaketti tarjoaa lukuisia erilaisia toimintoja ja vaihtoehtoja, jotka voivat auttaa muuttamaan päivämäärän haluttuun muotoon.

Esimerkiksi voit käyttää Add-funktiota lisätäksesi aikaa tiettyyn päivämäärään tai Sub-funktiota vähentääksesi sitä. Voit myös käyttää Truncate-menetelmää pyöristääksesi ajan haluttuun tarkkuuteen. Kaikilla näillä toiminnoilla on monia lisävaihtoehtoja, jotka voi löytää Go-ohjelmoinnin dokumentaatiosta.

## Katso myös

- Aikapaketin dokumentaatio: https://golang.org/pkg/time/
- Larry Rice, "Aikapaketin käyttö Go-kielessä": https://medium.com/@larryrice/golang-time-package-notes-7ff3a57ab562
- Go-kielen virallinen verkkosivusto: https://golang.org/