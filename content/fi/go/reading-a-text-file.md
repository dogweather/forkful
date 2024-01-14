---
title:    "Go: Tekstitiedoston lukeminen"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Lukeminen ja kirjoittaminen ovat peruspilareita ohjelmoinnissa, ja monesti meidän täytyy käsitellä tiedostojen syöttöä ja tulostusta. Tämä artikkeli käsittelee kuinka lukea tekstitiedostoja Go-kielellä ja tarjoaa esimerkkejä siitä, mitä voit tehdä tiedoston sisällön kanssa.

## Kuinka tehdä

Go-kielellä on helppo lukea tekstitiedostoja käyttäen `io/ioutil` -pakettia. Seuraavassa on yksinkertainen esimerkki, jossa luemme tekstitiedoston nimeltä `data.txt` ja tulostamme sen sisällön konsolille:

```Go
package main

import (
	"fmt"
	"io/ioutil"
)

func main() {

	// Luetaan tiedostosta sisältö ja tallennetaan muuttujaan
	data, err := ioutil.ReadFile("data.txt")
	if err != nil {
		fmt.Println("Virhe tiedoston lataamisessa:", err)
		return
	}

	// Tulostetaan tiedoston sisältö konsolille
	fmt.Println(string(data))
}
```

Tämän esimerkin avulla voit helposti lukea ja tulostaa minkä tahansa tekstitiedoston sisällön käyttäen Go-koodia.

## Syventävä syöksy

Go-kielellä luettu tekstitiedosto voidaan tallentaa muuttujaan merkkijonona, kuten esimerkissä yllä. Saat myös pääsyn tiedoston sisältöön kiinteärakenteisena, kuten `byte` tai `string` -slicing-käytettäväksi. Voit myös käyttää mukautettuja luku- ja kirjoitustoimintoja, kuten `bufio`-pakettia, jotta tiedoston käsittely olisi tehokkaampaa.

On myös tärkeää varmistaa, että käsittelet virheitä lukemisen aikana, kuten esimerkissä yllä. Voit lukea ja käsitellä tiedostoja käyttämällä useita eri paketteja Go-kielessä, joten kannattaa selvittää mikä sopii parhaiten tarpeisiisi.

## Katso myös

- [Go:n viralliset sivut](https://golang.org/)
- [Go:n `io/ioutil` -paketti](https://golang.org/pkg/io/ioutil/)
- [Go:n `bufio` -paketti](https://golang.org/pkg/bufio/)