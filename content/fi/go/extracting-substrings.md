---
title:                "Go: Alirivien erottelu"
simple_title:         "Alirivien erottelu"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit siis erottaa alimerkkijonoja Go-ohjelmoinnissa? Yksinkertaisesti sanottuna, alimerkkijonien erottaminen antaa sinulle mahdollisuuden käsitellä tiettyjä osia merkkijonoista erikseen. Tämän avulla voit suorittaa monimutkaisempia toimintoja ja muokata merkkijonoja haluamasi mukaisesti.

## Miten

```Go
package main

import "fmt"

func main() {
  s := "Tämä on esimerkki merkkijonosta"
  
  // Erota alimerkkijono tietystä indeksistä alkaen
  fmt.Println(s[5:]) // tulostaa "on esimerkki merkkijonosta"
  
  // Erota alimerkkijono tiettyjen indeksien välillä
  fmt.Println(s[5:9]) // tulostaa "on e"
  
  // Erota alimerkkijono tietystä indeksistä alkaen ja enintään tiettyyn indeksiin asti
  fmt.Println(s[:7]) // tulostaa "Tämä on"
}
```

Yllä oleva koodi näyttää, kuinka voit käyttää sisäänrakennettuja indeksointiominaisuuksia erottaaksesi alimerkkijonoja merkkijonosta. Muista, että merkkijonon indeksit alkavat nollasta.

## Syvällinen sukellus

Alimerkkijonien erottaminen voi olla erittäin hyödyllistä esimerkiksi, kun käsittelet käyttäjän antamia tekstisyötteitä. Jos haluat esimerkiksi tarkistaa, sisältääkö käyttäjän antama sähköpostiosoite @-merkin, voit erottaa osan merkkijonosta @-merkkiin asti ja tarkistaa, onko siinä oikea muoto.

```Go
package main

import "fmt"

func main() {
  var email string
  
  fmt.Print("Syötä sähköpostiosoite: ")
  fmt.Scan(&email)
  
  // Erota alimerkkijono ensimmäisen @-merkin kohdalta
  username := email[:strings.Index(email, "@")]
  
  // Tarkista käyttäjänimi
  if username != "" {
    fmt.Println("Sähköpostiosoite on oikeassa muodossa!")
  } else {
    fmt.Println("Sähköpostiosoite on virheellinen!")
  }
}
```

Huomaa, että tässä käytettiin myös `strings` -paketin `Index` -funktiota, joka palauttaa ensimmäisen esiintymän indeksin merkkijonossa. Voit myös käyttää muita Go:n sisäänrakennettuja merkkijonojen käsittelyfunktioita, kuten `Contains` ja `Replace`, joiden avulla voit tehdä monimutkaisempia alimerkkijonien erottamiseen liittyviä toimintoja.

## Katso myös

- [Go:n merkkijonojen käsittely](https://gobyexample.com/strings)
- [Go:n sisäänrakennetut merkkijonojen käsittelytoiminnot](https://golang.org/pkg/strings/)