---
title:                "C++: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittaminen testejä ohjelmistokehityksessä voi ensi näkemältä tuntua turhalta ja aikaa vievältä. Kuitenkin testien kirjoittaminen on tärkeä osa ohjelmiston laadun varmistamista ja vähentää virheiden riskiä tuotantokäytössä.

## Miten

Alla olevassa koodiesimerkissä nähdään yksinkertainen funktio, joka summaa kaksi lukua ja palauttaa niiden tuloksen.

```C++
int sum(int a, int b) {
    return a + b;
}
```

Testien kirjoittamisen lähtökohtana on yleensä ajatella erilaisia syötteitä ja niiden odotettua tulosta. Tässä tapauksessa voimme esimerkiksi testata funktiota summaamaan negatiivisia lukuja:

```C++
int result = sum(-5, -10);
// result = -15
```

Toinen hyvä testitapaus voisi olla testata funktiota summaamaan nollia:

```C++
int result = sum(0, 0);
// result = 0
```

Näiden testitapausten avulla voimme varmistaa, että funktio palauttaa odotetut tulokset erilaisilla syötteillä. Näiden lisäksi olisi tärkeää testata myös esimerkiksi virheelliset syötteet, kuten merkkijonojen antaminen numeerisille parametreille.

## Syvällisempi tarkastelu

Testien kirjoittaminen vaatii huolellisuutta ja ajattelua eri mahdollisista skenaarioista. Yksinkertaiset testit eivät aina riitä, vaan on tärkeää myös miettiä testien kattavuutta ja mahdollisia reunatapauksia.

Hyvät testit myös auttavat kehittäjää ymmärtämään koodinsa toimintaa ja havaitsemaan mahdolliset logiikkavirheet ennen niiden päätyminen tuotantokäyttöön.

## Katso myös

- [Testien kirjoittaminen: Miksi, mitä ja miten?](https://www.taitotalo.com/blogi/2017/11/07/testien-kirjoittaminen/)
- [Testauskulttuuri: miksi testata ja miten tehdä se oikein](https://www.softwaretestinghelp.com/fi/testing-culture/)
- [Testiasiantuntijoiden vinkit testien kirjoittamiseen](https://www.taitotalo.com/blogi/2019/06/27/testien-kirjoittamisen-iloa-ja-tuskaa-testiasiantuntijoiden-vinkkeja/)