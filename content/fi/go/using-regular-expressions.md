---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Go: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kukaan haluaisi käyttää säännöllisiä lausekkeita (regular expressions, tai lyhyemmin regex) Go-ohjelmoinnissa? 

Regexit ovat erittäin hyödyllisiä työkaluja tekstien käsittelyyn ja etsintään. Niiden avulla voit tarkasti määrätä, minkälaisia merkkijonoja haluat etsiä ja käsitellä. Kun olet oppinut käyttämään regexejä, saat suuren edun monimutkaisten tekstien käsittelyssä.

## Miten

Käytännön esimerkki: Haluamme tarkistaa, onko annettu teksti sähköpostiosoite. Voimme tehdä sen käyttämällä regexiä "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,6}$".

```Go
package main

import (
  "fmt"
  "regexp"
)

func main() {
  // Luodaan regex-objekti
  emailRegex := regexp.MustCompile("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,6}$")

  // Testataan esimerkkitekstiä
  testiTeksti := "example@email.com"

  // Testataan, täsmääkö regex annettuun tekstiin
  if emailRegex.MatchString(testiTeksti) {
    fmt.Println("Sähköpostiosoite on kelvollinen.")
  } else {
    fmt.Println("Virheellinen sähköpostiosoite.")
  }
}
```

Tulostus:

```
Sähköpostiosoite on kelvollinen.
```

## Syvempää tietoa

Regexejä voi käyttää monella eri tavalla Go-ohjelmoinnissa, mutta yleisimmät käyttötarkoitukset ovat tekstin etsiminen, korvaaminen ja jaottelu. 

Regex-objektin luominen voidaan tehdä esimerkiksi `regexp.MustCompile()`-funktion avulla. Tämän funktion parametrina annetaan haluttu regex-kaava merkkijonona. 

Regexin tarkistaminen annetusta tekstistä voidaan tehdä `MatchString()`-funktiolla. Tämä palauttaa totuusarvon, joka kertoo, täsmääkö regex annettuun tekstiin vai ei. 

Jotta Go-ohjelmat voisivat käsitellä ja muokata tekstejä, on regexit ensin muutettava `Regexp`-tyyppisestä objektista `Match`-tyyppiseksi objektiksi, joka sisältää halutun tekstin osan. Tämä voidaan tehdä esimerkiksi `FindString()`- tai `FindStringSubmatch()`-funktioilla.

## Katso myös

- [Go:n regexp-paketti](https://golang.org/pkg/regexp/)
- [Regex-tutoriaali](https://regexone.com/)