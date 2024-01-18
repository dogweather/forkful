---
title:                "Päiväyksen purkaminen merkkijonosta"
html_title:           "Go: Päiväyksen purkaminen merkkijonosta"
simple_title:         "Päiväyksen purkaminen merkkijonosta"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Päivämäärän parsiminen merkkijonosta tarkoittaa päivämäärän muuttamista ymmärrettävämpään muotoon, kuten numeroksi tai päivämääräksi. Ohjelmoijat tekevät tätä usein, kun he haluavat käsitellä päivämääriä tietokoneella tai verrata niitä toisiinsa.

## Kuinka:

Go-kielellä päivämäärän parsiminen merkkijonosta on helppoa ja kätevää. Se voidaan tehdä käyttämällä Go:n standardikirjaston aikapakkauksen Date()-funktiota. Alla on koodiesimerkki, jossa käytämme tätä funktiota ja tulostamme päivämäärän halutussa muodossa.

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	dateStr := "2020-11-05"
	date, err := time.Parse("2006-01-02", dateStr)
	
	if err != nil{
		fmt.Println("Virheellinen päivämäärä")
	}else{
		fmt.Println("Päivämäärä: ", date.Format("2.1.2006"))
	}
}
```

Tuloste:

```
Päivämäärä:  5.11.2020
```

## Syvempi sukellus:

Päivämäärän parsiminen merkkijonosta on ollut haasteellinen tehtävä ohjelmoinnin alkuaikoina. Ennen aikapakkausta, ohjelmoijien täytyi kirjoittaa oma koodi päivämäärän käsittelyyn, mikä aiheutti paljon virheitä ja oli aikaa vievää.

On olemassa myös muita vaihtoehtoja päivämäärän parsimiseen, kuten käyttää aikapakkauksen ParseAny()-funktiota tai jopa erillistä kirjastoa.

Go:n Date()-funktio käyttää ISO 8601 -standardeja päivämäärien parsimiseen, mikä on aikoinaan luotu helpottamaan päivämäärien käsittelyä eri ohjelmointikielillä. Tämä standardi määrittelee päivään liittyvät numeromuotoiset merkkijonot, kuten "2020-11-05", joka voidaan helposti parsia ja muuttaa haluttuun muotoon.

## Katso myös:

- [Go:n aikapakkaus dokumentaatio](https://golang.org/pkg/time/)
- [ISO 8601 standardin selitys](https://en.wikipedia.org/wiki/ISO_8601)