---
title:    "Go: Nykyisen päivämäärän hankkiminen"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Miksi
Monet ohjelmistokehittäjät tarvitsevat nykyisen päivämäärän ja kellonajan erilaisiin tarkoituksiin, kuten laskutukseen ja aikaleimojen tallentamiseen tiedostoihin. Go-ohjelmointikielellä on helppo hakea ja käsitellä ajankohtaista päivämäärää, ja tässä blogikirjoituksessa näytämme, miten se tehdään.

## Kuinka tehdä
Aloitetaan luomalla uusi Go-ohjelma ja tuomalla paketti "time", joka sisältää tarvittavat toiminnot nykyisen päivämäärän hakemiseen.

```Go
package main

import (
  "fmt"
  "time"
)

func main() {
  // Haetaan nykyinen paivamaara
  currentDate := time.Now()
  fmt.Println("Nykyinen paivamaara on: ", currentDate)
}
```

Kun suoritat tämän ohjelman, näet nykyisen päivämäärän ja kellonajan tulostettuna komentoriville.

```
$ go run current_date.go
Nykyinen paivamaara on: 2020-09-15 14:00:00.8033337 +0300 EEST m=+0.002999901
```

Voit myös muokata tulostettavan päivämäärän muotoa käyttämällä "Format" -toimintoa ja haluamaasi aikaleiman merkkijonoa.

```Go
package main

import (
  "fmt"
  "time"
)

func main() {
	// Haetaan nykyinen paivamaara
	currentDate := time.Now()
	// Tulostetaan paivamaara muodossa: 15-Sep-20
	fmt.Println("Nykyinen paivamaara on: ", currentDate.Format("02-Jan-06"))
}
```

```
$ go run current_date.go
Nykyinen paivamaara on: 15-Sep-20
```

## Syvällisempi sukellus
Go tarjoaa myös muita toimintoja nykyisen päivämäärän käsittelyyn, kuten päivämäärän lisäämiseen tai vähentämiseen tietyn ajanjakson verran. Voit tehdä tämän käyttämällä "Add" ja "Sub" -toimintoja ja antamalla niille halutun aikaleiman ja määrän. Esimerkiksi seuraava koodi lisää nykyiseen päivämäärään yhden päivän:

```Go
package main

import (
  "fmt"
  "time"
)

func main() {
  // Haetaan nykyinen paivamaara
  currentDate := time.Now()
  // Lisataan yksi paiva
  nextDay := currentDate.AddDate(0, 0, 1)
  fmt.Println("Huomenna on: ", nextDay.Format("02-Jan-06"))
}
```

```
$ go run current_date.go
Huomenna on: 16-Sep-20
```

Voit myös käyttää muita aikayksiköitä kuten kuukausia tai vuosia "AddDate" -toiminnolla. Tutustu Go-dokumentaatioon löytääksesi lisätietoja ja nähdäksesi kaikki mahdolliset aikayksiköt.

## Katso myös
* Go-dokumentaatio nykyisen päivämäärän hakemisesta: https://golang.org/pkg/time/#Time.Now
* Muotoilumallit aikaleimojen merkkijonoiksi: https://golang.org/pkg/time/#pkg-constants
* Esimerkkejä päivämäärän muokkaamisesta Go:lla: https://golangbyexample.com/modifying-datetime-golang/