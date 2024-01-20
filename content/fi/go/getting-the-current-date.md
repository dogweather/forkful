---
title:                "Nykyisen päivämäärän hankkiminen"
html_title:           "Haskell: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Päivämäärän haku Go:ssa: Mikä & Miksi?
Päivämäärän haku on toiminto, joka hakee aktiivisen järjestelmän tämänhetkisen päivämäärän. Ohjelmoijat tekevät tämän vastaavien tapahtumien ajanrekisteröinnin, lokiin kirjaamisen ja aikaperusteisten toimintojen mahdollistamiseksi.

# Kuinka tehdä?
Esimerkiksi, Go:ssa saat nykyisen päivämäärän seuraavasti:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	fmt.Println(time.Now())
}
```
Ohjelman ajaessa saamme tulosteena nykyisen ajan GMT:ssä (Greenwich Mean Time), esimerkiksi: `2022-04-14 15:01:51.6176565 +0000 UTC m=+0.000579401`.

# Syväluotaus
**Historiallinen konteksti**: Go (tunnetaan myös nimellä Golang) lanseerattiin vuonna 2007 ja on suunniteltu tekemään ohjelmointi helpoksi ja tuottavaksi. Luodaan tunnisteita luontaisen kalenteriajan tavanomaisten operaatioiden suorittamiseksi, kuten päivämäärän haku. 

**Vaihtoehtoja**: Go:ssa voit muokata päivämäärämuotoa seuraavasti:
```Go
func main() {
	t = time.Now()
	fmt.Println(t.Format("2006-01-02"))
}
```
Tämä tulostaa päivämäärän muodossa `YYYY-MM-DD`, kuten `2022-04-14`.

**Toteutustiedot**: Go:ssa päivämäärän haku on toteutettu aikapaketissa (`time package`) ja `Now`-funktio palauttaa nykyisen ajan sekuntin tarkkuudella.

# Katso myös
Lisätietoja päivämäärän hakutoiminnoista Go:ssa on saatavilla seuraavissa lähteissä:

- Go:n virallisen dokumentaation `time package`: [https://golang.org/pkg/time/](https://golang.org/pkg/time/)
- Go:n päivämäärien ja aikojen muotoilu: [https://gobyexample.com/time-formatting-parsing](https://gobyexample.com/time-formatting-parsing)
- Stack Overflow keskustelu Go:n päivämäärätoiminnoista: [https://stackoverflow.com/questions/20234104/how-to-format-current-time-using-a-yyyymmddhhmmss-format](https://stackoverflow.com/questions/20234104/how-to-format-current-time-using-a-yyyymmddhhmmss-format)