---
title:                "Gleam: Testien kirjoittaminen"
programming_language: "Gleam"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/writing-tests.md"
---

{{< edit_this_page >}}

Miksi: Miksi haluaisit kirjoittaa testejä?

Kirjoittamalla testejä voit varmistaa, että koodisi toimii halutulla tavalla ja välttyä mahdollisilta virheiltä ja bugeilta tulevaisuudessa.

## Miten tehdä:

Gleamilla voit helposti kirjoittaa ja suorittaa testejä koodisi integroinnin varmistamiseksi. Katso esimerkiksi seuraavaa koodilohkoa:

```Gleam
fn laske(n) {
    if n == 0 {
        0
    } else {
        n + laske(n - 1)
    }
}

test "laske testi" {
    expect(laske(5)).toEqual(15)
}
```

Tässä esimerkissä luodaan testi, joka varmistaa, että `laske`-funktio toimii odotetulla tavalla. Jos testi onnistuu, saat nähdä seuraavan outputin:

```
✅ laske testi
```

Voit myös muokata funktion ja testin parametreja ja odotettuja tuloksia tarpeen mukaan.

## Syvällinen tarkastelu

Testien kirjoittaminen on tärkeä osa ohjelmointia, sillä se auttaa varmistamaan luotettavan ja toimivan koodin. Kun kirjoitat testejä, muista ottaa huomioon seuraavat asiat:

- Testaa eri käyttötapauksia ja syötteitä varmistaaksesi, että koodisi käyttäytyy odotetulla tavalla.
- Pyri kirjoittamaan selkeä ja ymmärrettävä testikoodi, jotta mahdollisten virheiden löytäminen ja korjaaminen on helpompaa.
- Päivitä testejä aina kun teet muutoksia koodiin, jotta varmistat kaiken pysyvän toimivana.

## Katso myös

- [The Gleam testing library](https://github.com/gleam-lang/testing)
- [Unit testing in Gleam](https://gleam.run/book/testing.html#unit-testing)
- [Writing tests in Gleam](https://gleam.run/articles/unit-tests)