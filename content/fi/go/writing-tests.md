---
title:                "Go: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi testaaminen on tärkeää Go-ohjelmoinnissa?

Testaamisen kirjoittaminen on tärkeä osa Go-ohjelmointia, koska se auttaa löytämään ja korjaamaan virheitä ennen kuin ohjelma julkaistaan tuotantoon. Tämä auttaa välttämään käyttäjien kohtaamia ongelmia ja parantaa ohjelman laatua yleisesti.

## Kuinka kirjoittaa testejä Go-ohjelmissa

Testien kirjoittaminen Go-ohjelmissa on suhteellisen helppoa. Voit käyttää sisäänrakennettua "testing" -pakettia ja sen mukana tulevia toimintoja helpottaaksesi testien luomista. Alla on yksinkertainen esimerkki:

```
package main

import (
    "testing"
)

// Testataan kahden numeron yhteenlaskua
func TestAddition(t *testing.T) {
    result := 2 + 3
    if result != 5 {
        t.Errorf("Yhteenlasku väärin, odotettiin 5 mutta saatiinkin %d", result)
    }
}
```

Kuten näet, testifunktio aloitetaan "Test" -sanalla ja sen jälkeen tulee testattavan toiminnon nimi. Tämän jälkeen voit määrittää haluamasi testit ja niiden odotetut tulokset. Jos testi ei tuota odotettua lopputulosta, voit käyttää "t.Errorf()" -funktiota ilmoittamaan virheestä.

Suorittaaksesi testit, voit käyttää "go test" -komentoa terminaalissa. Tämä näyttää tulokset, ja jos kaikki testit suoritettiin onnistuneesti, saat viestin "ok" konsolista.

## Syvällisempi sukellus testien kirjoittamiseen

Testien kirjoittaminen Go-ohjelmissa tarjoaa paljon mahdollisuuksia. Voit käyttää erilaisia testaustyökaluja, kuten "goconvey" tai "testify", jotka helpottavat testien kirjoittamista ja näyttävät selkeämmin testien tuloksia.

Lisäksi voit käyttää "benchmarks" -toimintoa testaamaan ohjelmasi suorituskykyä ja optimoimaan sitä. Voit myös hyödyntää "mocking" -tekniikkaa simuloimaan ulkoisia riippuvuuksia testien aikana.

Testit myös auttavat dokumentoimaan koodiasi ja helpottavat muiden kehittäjien ymmärtämistä ja ylläpitämistä. Ne myös mahdollistavat ohjelmasi skaalautumisen ja kehittymisen luotettavasti.

## Katso myös

- [Go-kieltojen sisäänrakennettu "testing" -paketti](https://golang.org/pkg/testing/)
- [GoConvey - testaustyökalu Go-kielen projekteihin](https://github.com/smartystreets/goconvey)
- [Testify - helppokäyttöinen kirjasto testien kirjoittamiseen Go-kielen projekteissa](https://github.com/stretchr/testify)
- [Go-lang.orgin testauksen opas](https://golang.org/doc/code.html#Testing)