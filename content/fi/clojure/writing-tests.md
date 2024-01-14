---
title:                "Clojure: Testien kirjoittaminen"
programming_language: "Clojure"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittaessamme koodia tai ohjelmia usein kohtaamme tilanteita, joissa haluamme varmistaa sen toimivuuden ja välttää mahdolliset virheet. Tämä on erityisen tärkeää silloin, kun koodiin tehdään muutoksia tai sitä jaetaan muiden kanssa. Tässä on tärkeää hyötyä testien kirjoittamisesta.

## Miten

Testien kirjoittaminen Clojurella on helppoa ja nopeaa. Käytämme siihen "clojure.spec" kirjastoa, joka tarjoaa meille työkalut arvojen validointiin ja virheiden havaitsemiseen. Esimerkiksi, jos haluamme testata yksinkertaisen funktion, joka laskee kahden numeron summaa, voimme kirjoittaa testin seuraavasti:

```Clojure
(clojure.spec.test/instrument `+)
(is= 4 (+ 2 2)) ; tulisi palauttaa true
```

Kun ajamme tämän testin, ohjelma tulostaa meille testin tuloksen, joka kertoo, onko summa oikein vai ei.

## Syvällisempi sukellus

Testien kirjoittaminen on tärkeä osa koodin laadun varmistamista. Ne auttavat meitä havaitsemaan virheitä ja välttämään niitä ennen kuin koodi siirtyy tuotantoon. Clojuren "clojure.spec" kirjasto tarjoaa meille kattavan työkalun testien kirjoittamiseen ja suorittamiseen. Voimme myös kirjoittaa omia tarkistajia, jotka auttavat meitä löytämään tietyntyyppisiä virheitä ja varmistamaan, että koodi toimii oikein.

## Katso myös

- [Documentation for clojure.spec](https://clojure.org/guides/spec)
- [Guide for writing tests in Clojure](https://lambdaisland.com/blog/2019-03-07-generative-testing-in-clojure-part-2-spec-beyond-the-basics)
- [Examples of using clojure.spec to improve code quality](https://www.therealest.host/blog/testing/
- [Kurssi "Test Driven Development" Clojure-rakkailla](https://purelyfunctional.tv/courses/test-driven-development-in-clojure/)