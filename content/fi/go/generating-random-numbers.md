---
title:                "Satunnaisten numeroiden luominen"
html_title:           "PowerShell: Satunnaisten numeroiden luominen"
simple_title:         "Satunnaisten numeroiden luominen"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Generoi Satunnaislukuja Golla: Näin Se Käy
 

## Mikä ja Miksi?
Satunnaislukujen luominen on prosessi, jossa luodaan ennustamattomia numeroita. Ohjelmoijat tekevät näin moniin tarkoituksiin, kuten simulointeihin, peliohjelmointiin, salaukseen ja algoritmien testaukseen.

## Näin tehdään:
Go:n standardikirjasto tarjoaa `math/rand` -paketin, jolla voit generoida satunnaislukuja. Tässä on esimerkki siitä, miten se toimii:

```Go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	rand.Seed(time.Now().UnixNano())
	fmt.Println(rand.Intn(100))
}
```
Koodi yllä luo satunnaisen kokonaisluvun väliltä 0-99. `rand.Seed(time.Now().UnixNano())` alustaa satunnaislukugeneraattorin nykyisellä ajanhetkellä.

## Syvemmin:
Historiallisesti satunnaislukujen tuottamista pidettiin haasteena. Varhaisimmat menetelmät perustuivat fyysisiin prosesseihin, kuten noppien heittoon tai rulettipyörän pyöräyttämiseen. Nykyaikana ohjelmistot tuottavat "pseudosatunnaislukusarjoja", jotka näyttävät olevan satunnaisia.

Go:n `math/rand` -paketin alaisuudessa satunnaislukugeneraattorina käytetään kongruential generaattoria. Tämä on yksinkertainen ja tehokas menetelmä, mutta se ei sovellu kaikkiin käyttötapauksiin. Vaihtoehtoisesti voidaan käyttää myös salattuja satunnaislukugeneraattoreita, kuten `crypto/rand`.

On huomionarvoista, että ohjelmoijien pitää ymmärtää, miten eri generaattorit toimivat, ja valita oikea työkalu heidän tarpeisiinsa.

## Näistä lisää:
- Go:n virallinen dokumentaatio `math/rand` -paketista: https://golang.org/pkg/math/rand/
- Go:n virallinen dokumentaatio `crypto/rand` -paketista: https://golang.org/pkg/crypto/rand/
- Yksityiskohtainen artikkeli pseudosatunnaislukugeneraattorien tyypistä, jota Go käyttää: https://en.wikipedia.org/wiki/Linear_congruential_generator