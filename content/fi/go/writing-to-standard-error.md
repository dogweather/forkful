---
title:    "Go: Tietokoneohjelmoinnin artikkeli: Kirjoittaminen standardi virhekanavaan"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Miksi

Kirjoittaminen standardivirheeseen (standard error) on tärkeä osa Go-ohjelmointia. Se mahdollistaa virheiden havaitsemisen ja korjaamisen ohjelman suorituksen aikana.

## Miten

Standardivirheeseen kirjoittaminen on helppoa Go-kielellä. Voit käyttää standardikirjaston `fmt` -pakettia ja sen `Fprintln()` -funktiota. Katso esimerkkiä alla olevasta koodipätkästä:

```Go
package main

import "fmt"

func main() {
  fmt.Fprintln(os.Stderr, "Tämä on virheilmoitus standardivirheeseen")
}
```

Suorituksen tuloksena saat seuraavan tulosteen:

```
Tämä on virheilmoitus standardivirheeseen
```

## Syvä Sukellus

Kirjoittaessa standardivirheeseen, on tärkeää muistaa, että se on erillinen virta kuin tavallinen tulostus (`fmt.Println()`). Tämä tarkoittaa sitä, että vaikka kirjoittaisit virheilmoituksia standardivirheeseen, muu tulostus ei keskeydy. Voit myös ohjata standardivirheen esimerkiksi tiedostoon tai toiseen prosessiin.

## Katso myös
- [Go-standardikirjaston dokumentaatio](https://golang.org/pkg/)
- [Go-kielen perusteet](https://tour.golang.org/welcome)
- [Käytännön esimerkkejä Go-ohjelmoinnista](https://github.com/golang/go/wiki/Learn)