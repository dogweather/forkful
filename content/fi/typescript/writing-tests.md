---
title:                "TypeScript: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi Kirjoittaa Testejä

Testien kirjoittaminen on tärkeä osa hyvää ohjelmistokehitystä. Ne auttavat varmistamaan, että koodi toimii odotetulla tavalla ja vähentävät virheiden riskiä. Ne myös helpottavat uusien ominaisuuksien lisäämistä ja vanhojen parantamista, koska testit voivat havaita mahdolliset rikkomukset.

## Kuinka Kirjoittaa Testejä

Testien kirjoittaminen TypeScriptillä on helppoa ja monipuolista. Voit käyttää esimerkiksi suositeltuja testauskirjastoja, kuten Jest, Mocha tai Jasmine.

Alla olevassa koodiesimerkissä käytetään Jest-kirjastoa. Ensimmäiseksi tarvitset asennettuna Jestin ja TypeScriptin, jotka voidaan asentaa npm-paketinhallintajärjestelmällä. Seuraavaksi voit luoda uuden testitiedoston esimerkiksi nimeltään "math.test.ts" ja lisätä siihen seuraavan koodin:

````TypeScript
import { sum } from './math';

describe('Math tests', () => {
  test('sum function should return the correct value', () => {
    expect(sum(2, 3)).toEqual(5);
  });
});
````

Tässä koodissa ensimmäiseksi tuodaan käytettäväksi math.ts-tiedostosta `sum`-funktio. Sitten testit suoritetaan `describe`-blokissa, jossa määritellään testien ryhmän nimi. `test`-funktiossa verrataan sum-funktion palauttaman arvon oikeaan arvoon käyttäen Jest-kirjaston `toEqual`-metodia.

Voit ajaa testit komennolla `npm test` ja näet tuloksen, jossa kerrotaan, onko testi läpäissyt vai ei.

## Syvemmälle Testien Kirjoittamiseen

Testien kirjoittamisen syvällinen ymmärtäminen vaatii perehtymistä niiden eri osa-alueisiin, kuten unit-testaukseen, integraatiotestaukseen ja funktionaaliseen testaukseen. Unit-testauksessa testataan yksittäisiä komponentteja erillisinä, integraatiotestauksessa testataan eri komponenttien toimivuutta yhdessä ja funktionaalisessa testauksessa testataan järjestelmän käyttäytymistä loppukäyttäjän näkökulmasta.

Lisäksi testien kirjoittamisessa on hyvä noudattaa hyviä käytäntöjä, kuten antipatternien välttämistä ja testien ylläpidon huomioimista.

## Katso Myös
- [Jest](https://jestjs.io/)
- [Mocha](https://mochajs.org/)
- [Jasmine](https://jasmine.github.io/)