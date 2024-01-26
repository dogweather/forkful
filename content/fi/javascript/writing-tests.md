---
title:                "Testien kirjoittaminen"
html_title:           "Arduino: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Javascript-testaus tarkoittaa koodin toimivuuden varmistamista automatisoiduilla testeillä. Testit parantavat sovelluksen laatua ja antavat kehittäjille luottamusta koodimuutosten yhteydessä.

## Näin se tehdään:
```Javascript
// Asennetaan Jest-kirjasto komennolla: npm install --save-dev jest

// funktiot.js
function summa(a, b) {
  return a + b;
}
module.exports = summa;

// funktiot.test.js
const summa = require('./funktiot');

test('yksinkertainen summaus testi', () => {
  expect(summa(1, 2)).toBe(3);
});

// Suoritetaan testit komennolla: npm test
```
Testin suoritus tuottaa tuloksen, jossa näkyy, menikö testi läpi vai ei.

## Syväluotaus:
JavaScript-testaus on kehittynyt vuosien varrella matkallaan yksinkertaisista assert-lauseista monimutkaisiin testauskehyksiin. Jest ja Mocha ovat suosittuja työkaluja JavaScript-testauksessa. Testeillä voi olla useita muotoja, kuten yksikkötesteistä integraatiotesteihin ja E2E-testeihin, ja ne voidaan suorittaa erilaisilla konfiguraatioilla CI/CD-putkissa.

## Katso myös:
- Jestin dokumentaatio: [https://jestjs.io/docs/getting-started](https://jestjs.io/docs/getting-started)
- Mochan sivusto: [https://mochajs.org/](https://mochajs.org/)
- Artikkeli testausstrategioista JavaScript-projekteissa: [https://martinfowler.com/articles/microservice-testing/](https://martinfowler.com/articles/microservice-testing/)
