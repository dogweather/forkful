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

## Mitä ja miksi?

Testien kirjoittaminen on tärkeä osa ohjelmistojen kehittämistä. Se tarkoittaa, että kirjoitamme pieniä ohjelmia, joilla varmistamme pääohjelmamme toimivuuden. Testien kirjoittaminen auttaa meitä havaitsemaan mahdolliset virheet ja parantamaan koodimme laatua.

Toisin sanoen, testien kirjoittaminen on tapa varmistaa, että ohjelmamme tekee mitä sen pitäisi tehdä ja toimii halutulla tavalla. Se auttaa myös estämään varsinaisten ohjelmien virheitä ja parantamaan niiden suorituskykyä.

## Miten teet sen:

Gleamissa testien kirjoittaminen tapahtuu käyttäen kirjastoa nimeltä `gleam/test`. Tässä esimerkissä testaamme yksinkertaista funktiota, joka palauttaa annetun parametrin.

```
Gleam test "Testing my function" {
  assert.equal "Hello!" my_module.my_function("Hello!")
}
```

Tämä koodi testaa, että funktion `my_function` kutsuttaessa parametrina annettu merkkijono on sama kuin palautettu arvo. Jos testi epäonnistuu, saamme virheilmoituksen. Muutoin testi menee läpi ja voimme olla varmoja, että funktio toimii kuten pitäisi.

Testeissä voidaan myös käyttää `assert`-lauseita, joiden tarkoituksena on tarkistaa, että tietyt ehdot toteutuvat. Esimerkiksi `assert.true my_module.my_condition()` tarkistaa, että funktio `my_condition` palauttaa totuusarvon `true`.

## Syvemmälle:

Testien kirjoittaminen on ollut osa ohjelmistokehittämistä jo pitkään, ja siihen on kehitetty erilaisia menetelmiä ja työkaluja. Usein testeillä pyritään myös kattamaan mahdollisimman monta mahdollista skenaariota, joten kirjoittaminen ja ylläpitäminen vaatii aikaa ja resursseja.

Vaihtoehtoisia tapoja testata ohjelmia ovat muun muassa manuaalinen testaus ja hyväksymistestaus. Näissä testeissä ohjelmaa ajetaan ja tarkastetaan manuaalisesti eri skenaarioissa. Saatavilla on myös muita testaustyökaluja, kuten JUnit ja Selenium, jotka tarjoavat erilaisia testaustoiminnallisuuksia.

Gleamissa testien kirjoittaminen on helppoa ja selkeää, ja `gleam/test`-kirjasto tarjoaa monipuolisia työkaluja testien tekemiseen. Kannattaa kuitenkin muistaa, että pelkästään testien kirjoittaminen ei takaa virheetöntä koodia, vaan se on yksi osa laadukkaan ohjelmiston kehittämistä.

## Katso myös:

- [Junit.org](https://junit.org/)
- [Selenium.dev](https://www.selenium.dev/)