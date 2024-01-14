---
title:    "Go: Päivämäärien vertailu"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Miksi: Miksi vertailla kahta päivämäärää?

Päivämäärien vertailu voi olla tärkeää, kun käsitellään aikaperusteisia tietoja ohjelmoinnissa. Esimerkiksi tietokantakyselyissä tai aikaperusteisten tapahtumien käsittelyssä on usein tarve vertailla päivämääriä ja tehdä päätöksiä niiden perusteella.

## Kuinka tehdä: Koodiesimerkkejä ja tulosteita Go-koodilohkoissa.

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    date1 := time.Date(2021, time.October, 20, 0, 0, 0, 0, time.UTC)
    date2 := time.Date(2021, time.October, 21, 0, 0, 0, 0, time.UTC)

    if date1.Before(date2) {
        fmt.Printf("%v is before %v\n", date1, date2)
    } else if date1.After(date2) {
        fmt.Printf("%v is after %v\n", date1, date2)
    } else {
        fmt.Printf("%v is equal to %v\n", date1, date2)
    }
}
```

Tuloste:
```
2021-10-20 00:00:00 +0000 UTC is before 2021-10-21 00:00:00 +0000 UTC
```

## Syvällistä tietoa: Kahden päivämäärän vertailu.

Päivämäärien vertailussa kannattaa kiinnittää huomiota aikavyöhykkeisiin ja käyttää aikaperusteisia funktioita, kuten `Before()` ja `After()`. Lisäksi voidaan käyttää Unix-aikaleimoja tai vertaille suoraan päivämäärän arvoja. On myös tärkeää huomioida päivämäärien muotoilu ja tukea erilaisia formaatteja ohjelmassa.

## Katso myös:

- [https://golang.org/pkg/time/](https://golang.org/pkg/time/) (Go:n virallinen dokumentaatio aikaperusteisista funktioista)
- [https://programming.guide/go/compare-time-between-dates.html](https://programming.guide/go/compare-time-between-dates.html) (Vertailuesimerkkejä Go:lla)
- [https://opensource.com/article/21/2/go-time-operations](https://opensource.com/article/21/2/go-time-operations) (Aikaperusteisten toimintojen esittely Go:ssa)