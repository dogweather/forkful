---
title:    "Go: Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Usein ohjelmoinnissa on tarpeen laskea päiviä tulevaan tai menneeseen ajankohtaan. Tämä voi olla hyödyllistä esimerkiksi tapahtumien tai projektien suunnittelussa.

## Kuinka laskea päivämäärä tulevaisuudessa tai menneisyydessä?

Tässä blogipostauksessa opit kuinka käyttää Go-ohjelmointikieltä päivämäärien laskemiseen tulevaisuuteen tai menneisyyteen.

#### Laske päivä tulevaisuudessa

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	var lisays int = 45 //lisättävä määrä päiviä
	n := time.Now()
	tulevaPaiva := n.AddDate(0, 0, lisays) //lasketaan tuleva päivä
	fmt.Println(tulevaPaiva.Format("02-01-2006")) //tulostetaan päivämäärä halutussa formaatissa
}
```

Tulostus:
```
13-12-2021
```

#### Laske päivä menneisyydessä

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	var vahennys int = 30 //vähennettävä määrä päiviä
	n := time.Now()
	menneisyys := n.AddDate(0, 0, -vahennys) //lasketaan menneinen päivä
	fmt.Println(menneisyys.Format("2006-01-02")) //tulostetaan päivämäärä halutussa formaatissa
}
```

Tulostus:
```
2021-10-17
```

## Syvemmälle aiheeseen

Tässä esimerkeissä käytimme Go:n aikafunktioita. `Now()`-funktio palauttaa nykyhetken ajan, ja `AddDate()`-funktio mahdollistaa päivien lisäämisen tai vähentämisen.

Huomioi myös, että esimerkeissä päivämäärien tulostusformaattina käytettiin `Format()`-funktiota, jolla voit määrittää päivämäärän tulostusmuodon haluamallasi tavalla.

## Katso myös

- https://golang.org/pkg/time/#Time.AddDate
- https://golang.org/pkg/time/#Time.Format