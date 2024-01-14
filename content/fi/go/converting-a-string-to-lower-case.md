---
title:    "Go: Muuntaminen merkkijonoksi pienellä kirjoitettuna"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Useimmissa ohjelmointikielissä on sisäänrakennettu toiminto, joka muuntaa merkkijonon pieniksi kirjaimiksi. Tämä on hyödyllistä monissa tilanteissa, esimerkiksi vertailussa tai tietokannassa etsittäessä. Joten tässä artikkelissa opimme, miksi ja miten muuntaa merkkijono pieniksi kirjaimiksi Go-ohjelmoinnissa.

## Miten

### Golangissa

Go-kielessä on sisäänrakennettu toiminto "strings.ToLower", joka muuntaa merkkijonon pieniksi kirjaimiksi. Alla on esimerkki koodista, joka käyttää tätä toimintoa ja tulostaa uuden merkkijonon pienillä kirjaimilla.

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "Tervetuloa suomeen!"
	lower := strings.ToLower(str)
	fmt.Println(lower)
}
```

**Tulos:**

tervetuloa suomeen!

### Satunnaisten kirjainten muuntaminen

Jos haluat muuntaa vain tietyt kirjaimet pieniksi, voit käyttää "strings.Map" -toimintoa. Tämä toiminto ottaa kaksi parametria: muuntofunktion ja merkkijonon. Alla on esimerkki koodista, joka muuntaa merkkijonon kaikki "a"-kirjaimet pieniksi kirjaimiksi ja tulostaa uuden merkkijonon.

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "HauHau"
	lower := strings.Map(func(r rune) rune {
		if r == 'a' {
			return 'A'
		}
		return r
	}, str)
	fmt.Println(lower)
}
```

**Tulos:**

hAuhAu

## Syvempi sukellus

Go-kielessä merkkijonot ovat muuttumattomia, mikä tarkoittaa, että alkuperäinen merkkijono pysyy ennallaan ja uusi merkkijono palautetaan muuntotoiminnon jälkeen. Tämä on hyödyllistä, kun haluat säilyttää alkuperäisen merkkijonon ja käyttää muunnettua versiota. Lisäksi, Go-kielessä onnistuu myös kansainvälisten merkkien muuntaminen pieniksi kirjaimiksi.

## Katso myös

- [Go-julkaisu](https://golang.org/)
- [Golang virallinen dokumentaatio](https://golang.org/doc/)
- [Go-standardikirjasto](https://golang.org/pkg/)