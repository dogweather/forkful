---
title:                "Go: Kirjoittaminen standardivirheeseen"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi kirjoittaa standardiin virhevirtaan?

Standardi virhevirta on tärkeä osa Go-ohjelmointikieltä. Se antaa mahdollisuuden kirjoittaa virheviestejä ja muita ilmoituksia, jotka auttavat ohjelman kehittäjää paikantamaan ja korjaamaan mahdollisia ongelmia. Tässä blogikirjoituksessa kerromme mitä standardi virhevirta tarkoittaa ja miten sitä voidaan käyttää Go-koodissa.

## Miten kirjoittaa standardiin virhevirtaan?

Kun haluat kirjoittaa jotain standardiin virhevirtaan, voit käyttää fmt-paketin `Fprintln`-funktiota. Se hyväksyy standardi virhevirran `os.Stderr` parametrina ja kirjoittaa annetun viestin sinne. Katso esimerkki alla olevasta koodipätkästä:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	name := "Maija"
	err := fmt.Errorf("Tervetuloa, %s!", name)
	fmt.Fprintln(os.Stderr, err)
}
```
Tämän esimerkin tulostus näyttää seuraavalta: `Tervetuloa, Maija!`.

## Syväsukellus standardiin virhevirtaan

Go-koodissa on hyvä tapa käyttää standardi virhevirtaa silloin, kun haluat ilmoittaa käyttäjälle jostain poikkeavasta. Standardi virhevirta eroaa tavallisesta tulostamisesta siitä, että se näkyy käyttäjälle punaisella värillä ja osoittaa, että kyseessä on virheviesti. Tämä auttaa käyttäjää huomaamaan nopeasti, että jokin ohjelmassa on mennyt pieleen.

Toisin kuin tavallinen tulostaminen, standardiin virhevirtaan kirjoittaminen ei vaadi ohjelman suorituksen keskeyttämistä. Tämä tarkoittaa sitä, että ohjelma voi jatkua suorittamistaan myös virheen jälkeen.

## Katso myös

- [Go-kieen virallinen sivusto](https://golang.org/)
- [Go-kirjallisuus suomeksi](https://github.com/golang/go/wiki/Go-Can-Read).
- [Go-koodiesimerkkejä](https://github.com/golang/example).
- [Go-ohjelmointikielen perusteet](https://tour.golang.org/welcome/1).