---
title:                "Gleam: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/writing-tests.md"
---

{{< edit_this_page >}}

# Miksi Kirjoittaa Testejä?

Testaaminen on tärkeä osa ohjelmistokehitystä, sillä se varmistaa koodin toimivuuden ja vähentää virheiden riskiä. Testien kanssa työskentely myös auttaa kehittäjiä suunnittelemaan ja rakentamaan parempia ja luotettavampia ohjelmistoja.

# Kuinka Kirjoittaa Testejä Gleam-Ohjelmointikielillä?

Testien kirjoittaminen Gleam-ohjelmointikielillä on hyvin yksinkertaista ja intuitiivista. Ensimmäiseksi sinun tulee määritellä testitapaukset `test`-funktioilla ja sen jälkeen suorittaa testit `run`-funktiolla.

```Gleam
test "Summa-funktio palauttaa oikean tuloksen" {
  assert summa(2, 3) == 5
}
```

`assert`-lauseen avulla voit tarkistaa, että haluttu tulos vastaa odotettua. Tämän jälkeen voit suorittaa testit ja nähdä niiden tulokset:

```Gleam
run([summa_test])
```

Tämän esimerkin tulisi palauttaa seuraava tulos:

```
Trepcudsumma-funktio palauttaa oikean tuloksen
[OK] All 1 tests passed.
```

# Syvemmälle Testien Kirjoittamiseen

Testien kirjoittamiseen Gleam-ohjelmointikielillä liittyy paljon muitakin hyödyllisiä toimintoja, kuten esimerkiksi `case`-lauseet ja `setup`-funktiot. Nämä mahdollistavat monimutkaisempien testitapausten luomisen ja testien suorittamisen ennalta määritellyssä ympäristössä.

Voit myös suorittaa yksittäisiä testejä `run`-funktiolla antamalla halutun testifunktion parametrina.

```Gleam
run([summa_test], [summa_jaettu_jaollisella_test])
```

Syvemmälle Gleam-ohjelmointikielen testaamiseen ja sen tarjoamiin mahdollisuuksiin voit tutustua tarkemmin [Gleam-testausdokumentaatiosta](https://gleam.run/documentation/test/).

# Katso Myös

- [Gleam-testausdokumentaatio](https://gleam.run/documentation/test/)
- [Gleam-tutoriaali](https://gleam.run/tour/)