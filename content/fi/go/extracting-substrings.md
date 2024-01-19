---
title:                "Alimerkkijonojen poiminta"
html_title:           "Gleam: Alimerkkijonojen poiminta"
simple_title:         "Alimerkkijonojen poiminta"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Alimerkkijonon erottaminen on prosessi, jossa valitaan tietty osa merkkijonosta. Ohjelmoijat tekevät tämän usein datan käsittelyn ja analysoinnin helpottamiseksi.

## Näin se tehdään:

```Go
package main

import "fmt"

func main() {
	str := "Tämä on esimerkki merkkijonosta"
  
	subStr := str[5:14]
  
	fmt.Println(subStr)
}
```

Ohjelman tulostus on:

```Go
on esimerk
```

## Tarkempi tarkastelu

Historiallisesti alimerkkijonojen erottaminen on ollut osa ohjelmointia jo kauan. Se on tarpeellinen toiminto useissa tärkeissä tehtävissä, kuten tiedon kaivaminen merkkijonoista tai koodin optimointi.

Go-kielessä ei ole erityistä funktiota alimerkkijonojen erottamiseen - sen sijaan käytämme indeksointia. Kuten yllä olevasta esimerkistä nähdään, käytämme hakasulkusyntaksia (esimerkkiString[aloitusIndeksi:lopetusIndeksi]) alimerkkijonon erottamiseen. Tämä on kuitenkin vain yksi tapa; sen vaihtoehto on esimerkiksi käyttää built-in "strings" -paketin funktioita, kuten strings.Index tai strings.Split.

On tärkeää muistaa, että Go käyttää puoliväliin suljettua indeksointia, mikä tarkoittaa, että lopetusindeksi ei sisälly tulokseen. Muita yksityiskohtia ovat myös byte-ekvivalenttien sijasta rune-ekvivalenttien indeksien käyttäminen silloin kun merkkijono sisältää universaaleja merkkejä.

## Katso myös

- Go officaalinen dokumentaatio strings paketti: https://golang.org/pkg/strings/
- Go By Example, Strings: https://gobyexample.com/strings
- StackOverflow, How to get a substring in Go?: https://stackoverflow.com/questions/7864316/how-to-get-a-substring-in-go