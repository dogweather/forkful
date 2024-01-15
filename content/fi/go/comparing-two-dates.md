---
title:                "Kahden päivämäärän vertailu"
html_title:           "Go: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi
Miksi haluaisit vertailla kahta päivämäärää? Ei hätää, tämä artikkeli auttaa sinua ymmärtämään, miten voit tehdä sen Go-ohjelmointikielellä!

## Kuinka
Vertailemisen perusperiaatteet ovat samat useimmissa ohjelmointikielissä, mutta tässä artikkelissa keskitymme siihen, kuinka voit tehdä sen Go-kielellä.

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    // Luo kaksi aikaa, joita haluat vertailla
    aika1 := time.Date(2020, time.January, 1, 0, 0, 0, 0, time.UTC)
    aika2 := time.Date(2021, time.February, 15, 0, 0, 0, 0, time.UTC)

    // Vertaile aikoja käyttäen "Before", "After" ja "Equal" -funktioita
    fmt.Println(aika1.Before(aika2)) // true
    fmt.Println(aika1.After(aika2))  // false
    fmt.Println(aika1.Equal(aika2))  // false
}
```

Output:
```
true
false
false
```

## Syvällisempi tutkimus
Go-kielellä on myös muita hyödyllisiä funktioita, joilla voit vertailla kahta päivämäärää, kuten "Compare" ja "Day".

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    // Luo kaksi aikaa, joita haluat vertailla
    aika1 := time.Date(2020, time.January, 1, 0, 0, 0, 0, time.UTC)
    aika2 := time.Date(2021, time.January, 1, 0, 0, 0, 0, time.UTC)

    // Vertaile päivämäärän tasolla käyttäen "Day" -funktiota
    fmt.Println(aika1.Day() == aika2.Day()) // true

    // Vertaile tarkemmin käyttäen "Compare" -funktiota
    fmt.Println(aika1.Compare(aika2)) // -1, mikä tarkoittaa, että aika1 on ennen aika2:ta

    // Voit myös käyttää "Year" ja "Month" -funktioita tarkempien vertailujen tekemiseen
}
```

## Katso myös
- [time-paketti Go:n dokumentaatiossa](https://golang.org/pkg/time/)
- [Vertailla kahta päivämäärää muiden ohjelmointikielten avulla](https://www.guru99.com/compare-two-dates-go.html)