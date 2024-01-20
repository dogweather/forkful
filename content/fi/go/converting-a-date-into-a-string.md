---
title:                "Päivämäärän muuttaminen merkkijonoksi"
html_title:           "Go: Päivämäärän muuttaminen merkkijonoksi"
simple_title:         "Päivämäärän muuttaminen merkkijonoksi"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Päivämäärän muuttaminen merkkijonoksi eli *date to string* -muunnos on toiminto, jossa päivämäärä-objekti muunnetaan ihmisen luettavaksi tekstiksi. Ohjelmoijat tekevät tämän, jotta päivämäärien esittäminen ja käsittely olisi selkeämpää ja helpompaa.

## Kuinka:

Go:ssa päivämäärän saa muutettua merkkijonoksi `Time`-paketin `Format`-metodilla. Esimerkkikoodi näyttää tältä:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	pvm := time.Now()
	str := pvm.Format("2006-01-02 15:04:05")
	fmt.Println(str)
}
```

Ohjelman suoritus tulostaa nykyhetken päivämäärän ja kellon tietokoneen aikavyöhykkeellä.

## Syväsukellus:

Vuonna 2007 julkaistussa Go:n esiversiossa *date to string* -muunnos tehtiin käyttämällä **strftime**-funktiota. Tästä käytännöstä luovuttiin, sillä **Format**-menetelmä osoittautui tehokkaammaksi.

Go:n **Format**-metodin syntaksi eroaa muiden ohjelmointikielien vastaavista. Metodin parametrina oleva aika "2006-01-02 15:04:05" on Go:n kehittäjien mukaan helposti muistettava mnemoninen aika, jota käytetään kertomaan, mihin kohtaan lopullista merkkijonoa päivämäärän ja kellonajan osat sijoitetaan.

Vaihtoehtona tälle on esimerkiksi **Unix**-muotoisten aikaleimojen käyttö. Silloin päivämäärä esitetään sekunteina, jotka on kulunut tietystä ennalta määrätystä hetkestä, kuten "1970-01-01 00:00:00 UTC".

## Katso myös:

1. [Go:n ajan käsittelyn dokumentaatio](https://golang.org/pkg/time/)
2. [Go:n datan esittämisen dokumentaatio](https://golang.org/pkg/fmt/)
3. [strftime vs Format - keskustelu](https://stackoverflow.com/questions/20234104/how-to-format-current-time-using-a-yyyymmddhhmmss-format)