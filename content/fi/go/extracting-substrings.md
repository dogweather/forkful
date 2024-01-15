---
title:                "Alimerkkijonojen erottelu"
html_title:           "Go: Alimerkkijonojen erottelu"
simple_title:         "Alimerkkijonojen erottelu"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit erottaa alimerkkijonoja? Paljonko kertaa se on ollut tarpeen käytännössä ohjelmoidessa? Meillä kaikilla on varmasti ollut tilanteita, joissa olemme tarvinneet tietyistä merkkijonoista vain pienen osan, eikä koko merkkijonoa tarvita. Onneksi Go-kielellä on helppo tapa erottaa alimerkkijonoja merkkijonoista!

## Kuinka tehdä se

```Go
package main

import "fmt"

func main() {
    s := "Tämä on esimerkki lause."
    // Tulosta koko merkkijono
    fmt.Println(s)
    
    // Tulosta vain ensimmäiset 5 merkkiä
    fmt.Println(s[:5])
    
    // Tulosta merkkijonon viimeiset 7 merkkiä
    fmt.Println(s[len(s)-7:])
    
    // Tulosta merkkijonon kolmannesta charista kuudenteen chariin
    fmt.Println(s[2:6])
}
```

Tuloste:

```
Tämä on esimerkki lause.
Tämä 
 lause.
mä on
```

Olemme ensin määrittäneet muuttujan `s` ja annettu sille arvoksi esimerkkilauseen. Sitten olemme käyttäneet `[indeksi]` merkintää erottaaksemme alimerkkijonoja. Voit huomata, että Go-kielessä alimerkkijonon ensimmäinen merkki on indeksissä 0 ja viimeinen on `len(s)-1`.

## Syvällinen sukellus

Go-kielessä `string` tyyppiä käsitellään iteroitavina byteinä. Tämä tarkoittaa, että jokainen merkkiä vastaa Unicode code point, jota voidaan käsitellä erillisinä byteina. Tästä syystä voimme käyttää `[indeksi]` merkintää erottaaksemme alimerkkijonoja.

Voidaksesi käyttää alimerkkijonon ensimmäistä osaa, sinun täytyy määrittää indeksi `0` tai jättää se kokonaan pois. Samoin voit käyttää `[indeksi:]` merkintää määrittämättä toista arvoa, jotta saat loppuosan merkkijonosta.

Lisäksi Go-kielessä on myös `Rune` data tyyppi, joka vastaa Unicode code pointeja. Voit käyttää `range` rakennetta iteroimaan `string` ja `Rune` tyyppien yli, jos haluat käsitellä merkkejä yksitellen.

Go-kielellä on myös muita tapoja erottaa alimerkkijonoja, kuten `strings` paketin `Substring` funktio. Se ottaa parametreinä merkkijonon, alku- ja loppuindeksit ja palauttaa alimerkkijonon.

## Katso myös

- [Go-kielessä string-tyypin dokumentaatio](https://golang.org/pkg/strings/#Substring)
- [Go-kielessä Rune-tyypin dokumentaatio](https://golang.org/pkg/unicode/utf8/)