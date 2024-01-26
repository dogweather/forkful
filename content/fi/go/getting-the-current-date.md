---
title:                "Nykyisen päivämäärän hankkiminen"
date:                  2024-01-20T15:14:28.505810-07:00
html_title:           "Bash: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? 
Mikä ja mihin? Käsittelemme nykyisen päivämäärän hakemista. Ohjelmoijat tarvitsevat tätä usein, kuten aikaleimoja, ajan mittauksia tai päiväkohtaisia tehtäviä varten.

## How to:
Näin se tehdään:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	currentTime := time.Now()
	fmt.Println("Nykyinen päivämäärä ja aika:", currentTime)
}
```

Tuloste:

```
Nykyinen päivämäärä ja aika: 2023-03-04 15:04:05.123456 +0200 EET
```

## Deep Dive
Syväsukellus: `time.Now()` on Go-ohjelmoinnin standardikirjaston ajanottoa varten. Historiallisesti, päivämäärien ja aikojen käsittely on ollut eri ohjelmointikielissä hankalaa. Go tekee siitä suhteellisen suoraviivaista. Vaihtoehtoisia tapoja saada nykyinen päivämäärä: NTP-palvelimelta verkossa, käyttöjärjestelmän komentorivit tai ulkoiset kirjastot, kuten `github.com/arrow-py/arrow` Go:ssa. Implementation yksityiskohdissa on hyvä muistaa aikavyöhykkeet ja kesäaikaan siirtymiset.

## See Also
Katso myös:

- Go:n dokumentaatio: https://golang.org/pkg/time/
- Aikaan liittyvät ISO standardit: https://www.iso.org/iso-8601-date-and-time-format.html
- Tietoa NTP:stä: http://www.ntp.org/
- Arrow-kirjaston Go-versio: https://github.com/arrow-py/arrow
