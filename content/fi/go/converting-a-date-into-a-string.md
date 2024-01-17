---
title:                "Päivämäärän muuntaminen merkkijonoksi"
html_title:           "Go: Päivämäärän muuntaminen merkkijonoksi"
simple_title:         "Päivämäärän muuntaminen merkkijonoksi"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?
Mikäli olet ohjelmoija, saatat joutua muuttamaan päivämäärän merkkijonoksi koodissasi. Tämä voi olla tarpeellista esimerkiksi kun haluat tallentaa päivämäärän tietokantaan tai näyttää sen käyttäjälle ymmärrettävässä muodossa. Tämä muunnos on siis tärkeä osa ohjelmointia.

## Miten:
Go-kielessä päivämäärän muuntaminen merkkijonoksi on yksinkertaista. Alla esimerkki koodista ja sen tulos:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    date := time.Now().Format("2.1.2006")
    fmt.Println(date)
}
```
**Tulos: 2.1.2021**

## Syvempi sukellus:
Tämä päivämäärän muunnosmetodi perustuu "Go Time"-paketin `Format()` -metodiin. Tämä paketti sisältää myös muita hyödyllisiä työkaluja päivämäärän käsittelyyn. Toisena vaihtoehtona päivämäärän muuntamiselle merkkijonoksi voit myös käyttää `strconv`-pakettia. Tämä paketti tarjoaa enemmän kontrollia päivämäärän muunnoksessa ja on erityisen hyödyllinen kansainvälisten päivämäärämuotojen käsittelyssä.

## Katso myös:
[Go Time-paketin dokumentaatio](https://golang.org/pkg/time/)