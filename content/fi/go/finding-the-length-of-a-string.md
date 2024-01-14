---
title:    "Go: Merkkijonon pituuden löytäminen"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Miksi
Miksi haluaisit selvittää merkkijonon pituuden? Vastaus on yksinkertainen: merkkijonojen pituuden määrittäminen on tärkeä osa monia ohjelmointitehtäviä. Se auttaa esimerkiksi tarkistamaan, onko käyttäjän antama syöte oikean pituinen, tai laskemaan taulukon tai listan alkioiden määrän.

## Miten
Voit helposti selvittää merkkijonon pituuden käyttämällä `len()` funktiota Go-kielessä. Alla olevassa esimerkissä käytämme `len()` funktiota selvittääksemme kahden eri merkkijonon pituudet:

```Go
package main

import "fmt"

func main() {
  merkkijono1 := "Hei maailma!"
  merkkijono2 := "Tämä on toinen merkkijono."
  
  fmt.Println("Merkkijono1 pituus:", len(merkkijono1))
  fmt.Println("Merkkijono2 pituus:", len(merkkijono2))
}
```
Output:
```
Merkkijono1 pituus: 12
Merkkijono2 pituus: 26
```
## Syvällisemmin
Go-kielellä merkkijonot ovat \[merkkijono\] tyypin muuttujia, ja niitä käsitellään eri tavalla kuin esimerkiksi integer- tai boolean-muuttujia. Tästä syystä `len()` funktio on tärkeä apuväline merkkijonojen käsittelyssä. Lisäksi merkkijonojen merkit käyttävät tiettyä määrää tavuja, ja `len()` funktio laskee näiden tavujen määrän ja palauttaa sen pituuden.

Nyt kun tiedät miten `len()` funktiota käytetään merkkijonojen pituuden selvittämiseen, voit aloittaa harjoittelun ja soveltaa tätä tietoa omiin ohjelmointitehtäviisi.

## Katso myös
- [Go-kie