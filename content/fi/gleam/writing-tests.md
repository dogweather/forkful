---
title:                "Testien kirjoittaminen"
html_title:           "Gleam: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Testien kirjoittaminen voi tuntua turhalta ja aikaa vievältä, mutta se on kriittinen osa ohjelmoinnin prosessia. Testit varmistavat, että koodi toimii odotetulla tavalla ja auttavat löytämään mahdolliset virheet ja bugeja ennen kuin ne pääsevät käyttöön.

## Miten

```Gleam
test "Tunnistaa kestotyötyypin"
  expect(Koodi.työtyyppi(5)).toBe("Kesto")
```

Ensimmäinen askel testien kirjoittamisessa on määrittää, mitä haluat testata. Tämä tapahtuu testilausekkeilla, jotka käyttävät `test` ja `expect` avainsanoja. Voit myös antaa testeille kuvaavia nimiä, kuten yllä oleva esimerkki osoittaa. Kirjoita koodi, joka testaa odotettua toiminnallisuutta ja käytä `toBe` metodia varmistaaksesi, että toiminto palauttaa oikean arvon.

## Syväsukellus

Testien kirjoittaminen ei ole vain koodin toiminnallisuuden tarkistamista, vaan myös tapa varmistaa, että koodi pysyy toiminnassa tulevaisuudessakin. Hyvien testien avulla voit havaita mahdolliset säröt ja muutokset, jotka voivat vaikuttaa koodin toimintaan. Näin voit minimoida virheiden määrän ja säästää aikaa ja vaivaa tulevassa kehityksessä.

## Katso myös

- [Gleam-ohjelmointikielen virallinen sivusto](https://gleam.run/)
- [Gleam-testausdokumentaatio](https://gleam.run/documentation/testing)
- [Gleam-testilibraryn käyttöohjeet](https://github.com/gleam-lang/gleam/blob/master/lib/testing/README.md)