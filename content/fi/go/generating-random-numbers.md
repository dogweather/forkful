---
title:                "Go: Sattumanvaraisten numeroiden tuottaminen"
simple_title:         "Sattumanvaraisten numeroiden tuottaminen"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

Monta kertaa ohjelmoinnin aikana tarvitsemme satunnaisia numeroita. Esimerkiksi simulaatioissa, arvonnassa tai pelien kehittämisessä. Onneksi Go-kielessä on helppo tapa generoida satunnaisluvut tarpeidemme mukaan.

## Miten

Go-kielessä satunnaisluvut generoidaan rand-paketin avulla. Käytettävä funktio on `Intn`, joka ottaa parametrikseen halutun maksimiarvon ja palauttaa satunnaisen kokonaisluvun väliltä 0 ja annettu maksimiarvo -1 välillä.

```Go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	rand.Seed(time.Now().UnixNano()) // Asetetaan satunnainen siemen jokaisella suorituskerralla
	fmt.Println(rand.Intn(100)) // Generoidaan satunnainen luku väliltä 0-99
}
```

Esimerkissä käytämme myös `Seed`-funktiota, joka asettaa satunnaisen siemenen jokaisen suorituksen alussa. Tämä tekee satunnaisluvuista vieläkin satunnaisempia.

## Syvemmälle

Go-kielessä tapahtuu monia taustaprosesseja satunnaisluvun generoimisen aikana. Ensinnäkin se luo pseudo-satunnaisen sekvenssin käyttämällä Xorshift-algoritmia. Tämä algoritmi on nopea ja tuottaa hyvin jakautuneita satunnaislukuja.

Toiseksi, `Seed`-funktio käyttää Unix-aikaa satunnaisen siemenen luomiseksi. Tämä takaa, että jokainen suoritus saa erilaisen satunnaisen sekvenssin.

Lisäksi, jos haluat generoida satunnaislukuja jonkin tietyn algoritmin mukaan, voit luoda oman `Source`-tyypin ja käyttää sitä `New`-funktion avulla.

## Katso myös

- Dokumentointi: https://golang.org/pkg/math/rand/
- Pseudo-satunnainen sekvenssi Xorshift-algoritmillä: https://en.wikipedia.org/wiki/Xorshift
- Ohjeet satunnaisen siementämisen parhaista käytännöistä: https://stackoverflow.com/questions/12321133/golang-random-number-generator-how-to-seed-properly