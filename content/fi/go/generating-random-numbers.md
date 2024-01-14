---
title:    "Go: Satunnaislukujen generointi"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmoinnin tehtävissä, erityisesti pelikehityksessä ja tietoturvassa, tarvitaan satunnaisia numeroita. Näiden lukujen generoiminen voi auttaa lisäämään monimuotoisuutta ja arvaamattomuutta ohjelmiin. 

## Kuinka tehdä

Go-kielellä satunnaisia numeroita voidaan luoda käyttämällä "rand" -paketin "Intn" -funktiota. Tämä funktio ottaa parametrina kokonaisluvun ja palauttaa satunnaisen luvun väliltä 0 ja parametrina annetun luvun väliltä. Esimerkiksi jos haluamme generoida satunnaisen luvun väliltä 1-10, voimme käyttää seuraavaa koodia:

```Go 

package main

import (
    "fmt"
    "math/rand"
    "time"
)

func main() {
    rand.Seed(time.Now().UnixNano())
    num := rand.Intn(10) + 1
    fmt.Println("Satunnainen luku 1-10 välillä:", num)
}
```

Koodissa käytetään myös "Seed" -funktiota, joka asettaa satunnaislukugeneraattorin lähtöarvon ajassa olevaan nanosekuntiin, jotta jokainen kerta koodin suorittamisen yhteydessä saadaan erilaisia satunnaisia lukuja.

Jos haluamme luoda satunnaisia desimaalilukuja, voimme käyttää "Float64" -funktiota, joka palauttaa satunnaisen luvun väliltä 0 ja 1. Esimerkiksi:

```Go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

func main() {
    rand.Seed(time.Now().UnixNano())
    num := rand.Float64()
    fmt.Println("Satunnainen desimaaliluku välillä 0-1:", num)
}
```

## Syväsukellus

Satunnaislukugeneraattori Go-kielessä perustuu aina lähtöarvoon, joka määritetään "Seed" -funktiolla. Tämän vuoksi, jos käytämme samaa lähtöarvoa, saamme aina samat satunnaiset luvut. Tämän välttämiseksi voimme vaihdella lähtöarvoa käyttämällä esimerkiksi ajan sijasta esimerkiksi ohjelman suoritusaikaa.

## Katso myös

- Go:n virallinen dokumentaatio satunnaislukujen generoimiseen: https://golang.org/pkg/math/rand/
- Satunnaislukujen käyttö tietoturvassa: https://www.ssh.com/vulnerability/random-number-generator/
- Satunnaislukujen merkitys pelikehityksessä: https://www.gamedesigning.org/learn/randomness/