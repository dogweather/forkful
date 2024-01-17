---
title:                "Testien kirjoittaminen"
html_title:           "Javascript: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Ohjelmistokehittäjät käyttävät nykyaikana yhä enemmän testeille omistettua aikaa ja resursseja, jotta varmistetaan koodin laatu ja toimivuus. Testaamisen tarkoituksena on havaita mahdollisia virheitä ja vikoja ohjelmistossa ennen sen julkaisemista, mikä puolestaan säästää aikaa ja vaivaa virheiden korjaamisessa myöhemmin.

## Miten?
Testien kirjoittaminen Javascriptillä on suhteellisen helppoa ja nopeaa. Se voidaan tehdä hyödyntämällä erilaisia kirjastoja, kuten esimerkiksi Jestiä tai Mochaa. Kirjastojen avulla voit luoda yksikkö-, integraatio- ja toiminnallisia testejä, jotka tarjoavat kattavan kattavuuden koodillesi.

```
function sum(a, b) {
  return a + b;
}

test('sum funktio laskee summan oikein', () => {
  expect(sum(2, 3)).toBe(5);
});
```

Testikoodi suoritetaan ajamalla testitiedosto erikseen tai käyttämällä automaattista testaamista, joka tarkistaa koodin jokaisen muutoksen jälkeen. Tämä auttaa havaitsemaan mahdollisia virheitä ja varmistaa, että koodi toimii kuten odotetaan.

## Syvemmälle
Testien kirjoittamista on käytetty ohjelmistokehityksessä jo vuosikymmeniä, ja se on vakiintunut käytäntö ketterien menetelmien yhteydessä. Vaikka manuaalinen testaaminen on edelleen tärkeää, automaattisten testien käyttöönotto on lisännyt kehittäjien tuottavuutta ja auttanut parantamaan ohjelmistojen laatua.

Vaihtoehtoisia tapoja testaamiseen ovat esimerkiksi BDD (Behaviour-driven development) ja TDD (Test-driven development), joissa testit kirjoitetaan ennen varsinaista koodia. Nämä lähestymistavat auttavat suunnittelemaan koodia ja varmistavat, että tarvittavat toiminnot ovat testattuja.

## Katso myös
- Jest: https://jestjs.io/
- Mocha: https://mochajs.org/
- BDD: https://en.wikipedia.org/wiki/Behavior-driven_development
- TDD: https://en.wikipedia.org/wiki/Test-driven_development