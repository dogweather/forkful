---
title:                "Merkkijonon interpolointi"
html_title:           "Bash: Merkkijonon interpolointi"
simple_title:         "Merkkijonon interpolointi"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Merkkijonon interpolointi on programattavuuden tekniikka, jossa muuttujan arvo sijoitetaan suoraan merkkijonojen sisään. Ohjelmoijat tekevät tätä helpottaakseen luettavan ja hallittavan koodin kirjoittamista.

## Näin se tehdään:

Katsotaan esimerkkiä Go:n Fmt-paketin `Sprintf`-funktion avulla:

```Go
package main
import "fmt"

func main() {
    name := "Pekka"
    age := 32
    s := fmt.Sprintf("%s on %d vuotta vanha.", name, age)

    fmt.Println(s)
}
```

Kun tämä esimerkkikoodi ajetaan, tulostetaan `Pekka on 32 vuotta vanha.`

## Syvällisempi sukellus:

Merkkijonon interpolointi on yleinen ominaisuus monissa ohjelmointikielissä palaten asti 1970-luvulle. Go:ssa `Sprintf`-funktiota käytetään, mutta jotkut muut kielet (kuten Python tai JavaScript) tarjoavat lisäsyntaksiä tätä varten.

Vaihtoehtoisesti Go:ssa voidaan käyttää `+` operaattoria merkkijonojen liittämiseen, mutta tämä ei ole yhtä suoraviivaista kuin interpolointi.

`Sprintf`-funktion toteutuksessa käytetään monimutkaista seurantajärjestelmää muuttujien tyyppejä ja arvoja varten. Se käy läpi merkkijonon, etsii muotoilukomennot (kuten `%s` tai `%d`), ja korvaa ne argumenttilistan vastaavalla arvolla.

## Katso myös:

- Fmt-paketti Go:n standardikirjastossa: [https://golang.org/pkg/fmt/](https://golang.org/pkg/fmt/)
- Lisää merkkijonon käsittely menetelmistä Go:ssa: [https://gobyexample.com/string-formatting](https://gobyexample.com/string-formatting)
- Yksityiskohtainen katsaus merkkijonon interpoloinnin historiaan: [https://en.wikipedia.org/wiki/String_interpolation](https://en.wikipedia.org/wiki/String_interpolation)