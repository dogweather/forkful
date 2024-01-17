---
title:                "Satunnaisten numeroiden luominen"
html_title:           "Go: Satunnaisten numeroiden luominen"
simple_title:         "Satunnaisten numeroiden luominen"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Satunnaislukujen luominen tarkoittaa satunnaisten numeroiden generoimista tietokoneohjelmassa. Ohjelmoijat käyttävät tätä toimintoa esimerkiksi simulaatioissa, peleissä ja salauksessa.

## Miten:

Go-ohjelmoinnissa satunnaislukuja voi luoda käyttämällä pakettia `math/rand` ja sen `Intn(n)` -funktiota, joka generoi satunnaisen kokonaisluvun välillä 0-n. 
Esimerkiksi:

```
Go funktiolla rand.Intn(100) voimme luoda satunnaisia lukuja välillä 0-100.

package main

import (
	"fmt"
	"math/rand"
)

func main() {
	randomNumber := rand.Intn(100)
	fmt.Println("Satunnainen luku välillä 0-100 on:", randomNumber)
}

```
Esimerkkitulos:

`Satunnainen luku välillä 0-100 on: 75`

## Syväsukellus:

Satunnaislukujen generoiminen on ollut tärkeä osa tietokoneohjelmointia jo pitkään. Aluksi ohjelmoijat joutuivat itse suunnittelemaan algoritmejä satunnaislukujen generoimiseksi, mutta nykyään monet ohjelmointikielet, kuten Go, tarjoavat valmiita funktioita tähän tarkoitukseen. 

Go:ssa on myös mahdollista käyttää `crypto/rand` -pakettia, joka tarjoaa turvallisempia satunnaislukuja, jotka perustuvat laadukkaisiin satunnaisiin lähdeaineistoihin.

## Katso myös:

- Dokumentaatio Go:n `math/rand` -paketista: https://golang.org/pkg/math/rand/
- Dokumentaatio Go:n `crypto/rand` -paketista: https://golang.org/pkg/crypto/rand/
- Artikkeli satunnaislukujen generoinnista Go:ssa: https://gobyexample.com/random-numbers