---
title:                "Testausten kirjoittaminen"
html_title:           "Haskell: Testausten kirjoittaminen"
simple_title:         "Testausten kirjoittaminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/writing-tests.md"
---

{{< edit_this_page >}}

Testing code is an essential practice for programmers, helping to ensure the quality and functionality of their code. By writing and running tests, programmers can catch bugs and errors early on and ensure their code is working as intended. 

## Mitä ja miksi?

Testien kirjoittaminen on prosessi, jossa ohjelmoijat luovat koodeja, joiden avulla he voivat tarkistaa ohjelmansa toimivuuden ja laadun. Tämä on tärkeä käytäntö, joka auttaa estämään virheitä ja varmistamaan koodin toimivuuden. 

## Kuinka:

Esimerkiksi, jos haluat testata yksinkertaisen laskutoimituksen, voit käyttää seuraavaa koodia:
```
Haskell
testi = 2+2 
```
Tämän jälkeen voit ajaa koodin, ja jos testi palauttaa oikean vastauksen, eli 4, niin voit olla varma, että laskutoimitus toimii oikein.

## Syvemmälle:

Testaus on ollut tärkeä osa ohjelmointia jo pitkään, ja monilla ohjelmointikielillä on omat testikehysjärjestelmänsä, kuten esimerkiksi Haskellin Hspec. Vaihtoehtoina testien kirjoittamiselle voi olla manuaalinen testaus tai käyttäjien antama palaute. 

Testaamisen toteutus riippuu ohjelman ja sen tavoitteiden luonteesta. On tärkeää, että testit kattavat mahdollisimman paljon koodia ja erilaisia syötteitä. Testien kirjoittaminen voi myös auttaa löytämään mahdollisia virheitä tai puutteita ohjelmassa, ja antaa näin mahdollisuuden parantaa sen toimivuutta ja luotettavuutta.

## Katso myös:

- Lisätietoja Haskellin Hspec-testikehysjärjestelmästä: https://hspec.github.io/
- Testausohjeita ja vinkkejä Haskell-koodaukseen: https://haskelltest.com/