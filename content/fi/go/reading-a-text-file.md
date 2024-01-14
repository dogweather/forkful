---
title:    "Go: Lukeminen tekstitiedostosta"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Miksi

Tekstin luku on olennainen osa monia ohjelmointitehtäviä, kuten tekstin analysointia ja tietojen käsittelyä. Tässä blogipostauksessa opit, miten voit lukea tekstitiedostoja Go-kielellä ja miten hyödyntää tätä taitoa projekteissasi.

## Miten

```Go 
package main 

import (
	"fmt"
	"io/ioutil"
)

func main() {
	// Avataan tiedosto "lauseet.txt" ja talletetaan se muuttujaan "tiedosto"
	tiedosto, virhe := ioutil.ReadFile("lauseet.txt")
	if virhe != nil {
		fmt.Println("Virhe tiedoston lukemisessa:", virhe)
	} else {
		// Tulostetaan tiedoston sisältö
		fmt.Println(string(tiedosto))
	}
}
```

Koodiesimerkki näyttää, miten voit avata ja lukea tekstitiedoston Go-kielellä. Ensimmäisessä rivissä tuodaan paketit "fmt" ja "io/ioutil" käyttöön. Tämän jälkeen tiedosto avataan käyttäen "ioutil.ReadFile" -funktiota ja sen sisältö talletetaan muuttujaan "tiedosto". Jos tiedoston lukemisessa ilmenee virhe, sitä käsitellään if-lauseen avulla. Muussa tapauksessa tiedoston sisältö tulostetaan käyttäen "fmt.Println()" -funktiota ja muutetaan binaaridata merkkijonoksi käyttäen "string()" -funktiota.

## Syväsukellus

Go tarjoaa myös muita tapoja lukea tekstitiedostoja, kuten käyttämällä "os.Open()" -funktiota tai "bufio" -pakettia. Voit myös tarkastella vaihtoehtoja tiedostosta lukemisen yhteydessä, kuten tiedoston merkistökoodauksen asettamista tai tietyn rivin tai merkkijonon lukemista.

## Katso myös

- [Go:n virallinen dokumentaatio tiedostojen lukemisesta](https://golang.org/pkg/os/#Open)
- [Go:n virallinen dokumentaatio bufio-paketista](https://golang.org/pkg/bufio/)
- [Go:n virallinen dokumentaatio string-paketista](https://golang.org/pkg/strings/)