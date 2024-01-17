---
title:                "Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
html_title:           "Go: Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
simple_title:         "Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen tarkoittaa päivämäärien ja ajanmäärien laskemista tietyllä ajanjaksolla. Tämä on tärkeä osa ohjelmointia, sillä monet ohjelmat tarvitsevat tietoa ajasta ja päivämäärästä. Joten ohjelmoijat käyttävät päivämäärien laskemista varmistaakseen, että heidän ohjelmansa toimivat oikein ajasta riippumatta.

## Kuinka se tehdään:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	now := time.Now()
	dayPlus := now.AddDate(0, 0, 1)
	dayMinus := now.AddDate(0, 0, -1)
	fmt.Println("Huominen: ", dayPlus)
	fmt.Println("Eilinen: ", dayMinus)
}
```

Tämä koodi laskee huomisen ja eilisen päivämäärän perustuen nykyiseen päivämäärään.

```
Output:
Huominen: 2021-12-13 09:00:00
Eilinen: 2021-12-11 09:00:00
```

## Syvemmälle:

Päivämäärän laskemisella on pitkät juuret, ja sitä on käytetty jo antiikin ajoista lähtien. Nykyaikaisessa ohjelmoinnissa on useita vaihtoehtoja päivämäärän laskemiselle, kuten käyttämällä Unix-timestampia tai erilaisia ​​kirjastoja. Go:lla on myös muita tehokkaita päivämäärä- ja aikatoimintoja, joita kannattaa tutkia.

## Katso myös:

- [Go:n virallinen dokumentaatio](https://golang.org/pkg/time/#Time.AddDate)
- [Päivämäärien laskemisen historia](https://www.sciencemag.org/careers/2002/08/calendars-and-past)
- [Unix-timestampin käyttäminen ajan laskemisessa](https://www.guru99.com/how-to-convert-unix-timestamp-to-date.html)