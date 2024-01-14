---
title:                "Go: Tarkista, onko hakemisto olemassa"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi tarkistaa, onko hakemisto olemassa?

Tiedostojen ja hakemistojen hallinnointi on tärkeä osa ohjelmointia, ja joskus on tarpeen tarkistaa, onko tietty hakemisto olemassa ennen kuin suoritetaan tiettyjä toimintoja. Tämä voi auttaa välttämään virheitä ja epämiellyttäviä tilanteita.

## Kuinka tarkistaa, onko hakemisto olemassa – esimerkkejä koodilla

Tässä blogipostissa tarkastelemme, kuinka Go-kielellä voi tarkistaa, onko hakemisto olemassa. Käytämme tähän osaksi osaa "os" -pakettia, joka tarjoaa toimintoja käyttöjärjestelmän tiedostojen ja hakemistojen hallintaan.

```Go
package main

import "os"
import "fmt"

func main() {

	// Tarkistetaan, onko hakemisto olemassa
	_, err := os.Stat("hakemisto")

	// Tulostetaan tulos
	if err == nil {
		fmt.Println("Hakemisto on olemassa")
	} else if os.IsNotExist(err) {
		fmt.Println("Hakemistoa ei ole olemassa")
	} else {
		fmt.Println("Jokin meni pieleen")
	}

}
```

Tässä koodissa käytämme "os.Stat()" -funktiota tarkistaaksemme, onko hakemisto nimeltä "hakemisto" olemassa. Jos "os.Stat()" palauttaa virheen, tarkistamme "os.IsNotExist()" -funktiolla, onko kyseessä hakemiston olemattomuus. Mikäli ei, tulostamme yleisen virheilmoituksen.

Toinen tapa tarkistaa hakemiston olemassaolo on käyttää "os.Open()" -funktiota ja yrittää avata hakemiston tiedostona. Jos tämä onnistuu, voimme olettaa, että kyseessä on hakemisto. Tässä esimerkki:

```Go
package main

import "os"
import "fmt"

func main() {

	// Yritetään avata hakemisto tiedostona
	_, err := os.Open("hakemisto")

	// Tulostetaan tulos
	if err == nil {
		fmt.Println("Hakemisto on olemassa")
	} else if os.IsNotExist(err) {
		fmt.Println("Hakemistoa ei ole olemassa")
	} else {
		fmt.Println("Jokin meni pieleen")
	}

}
```

Tämä lähestymistapa toimii myös mille tahansa tiedostolle, ei vain hakemistolle. Mikäli tiedosto on olemassa, "os.Open()" -funktio palauttaa tiedoston kahva, ja muussa tapauksessa virheen.

## Syventävä tietoa hakemiston olemassaolon tarkistamisesta

Mikäli haluat syventää tietämystäsi hakemiston olemassaolon tarkistamisesta, suosittelemme tutustumaan "os" -paketin dokumentaatioon osoitteessa https://golang.org/pkg/os/. Sieltä löydät lisätietoa erilaisista tavoista tarkistaa tiedostojen ja hakemistojen olemassaolo.

## Katso myös

- Lataa Go-kielen kehitysympäristö: https://golang.org/dl/
- "os" -pakettin dokumentaatio: https://golang.org/pkg/os/
- Artikkeli: "Kuinka luoda uusi hakemisto Go-kielellä": (lisää linkki artikkeliin)