---
aliases:
- /fi/javascript/writing-tests/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:22.423840-07:00
description: "Testien kirjoittaminen JavaScriptill\xE4 viittaa automatisoitujen skriptien\
  \ luomiseen, jotka ajavat koodiasi varmistaakseen sen toimivan odotetulla tavalla.\u2026"
lastmod: 2024-02-18 23:09:08.039439
model: gpt-4-0125-preview
summary: "Testien kirjoittaminen JavaScriptill\xE4 viittaa automatisoitujen skriptien\
  \ luomiseen, jotka ajavat koodiasi varmistaakseen sen toimivan odotetulla tavalla.\u2026"
title: Testien kirjoittaminen
---

{{< edit_this_page >}}

## Mitä & Miksi?

Testien kirjoittaminen JavaScriptillä viittaa automatisoitujen skriptien luomiseen, jotka ajavat koodiasi varmistaakseen sen toimivan odotetulla tavalla. Tämä voi merkittävästi parantaa sovellustesi luotettavuutta ja ylläpidettävyyttä. Ohjelmoijat tekevät tämän virheiden varhaisen havaitsemisen, koodin refaktoroinnin helpottamisen ja uusien ominaisuuksien varmistamisen, etteivät ne riko olemassa olevaa toiminnallisuutta, vuoksi.

## Miten:

### Natiivi lähestymistapa (käyttäen Jest)

Jest on suosittu testauskehys, joka tarjoaa ystävällisen API:n yksikkötestien kirjoittamiseen JavaScriptillä. Se vaatii minimaalisen konfiguraation ja sisältää ominaisuuksia kuten mock-funktiot, ajastimet ja tilannevedostestauksen.

1. **Asennus**:

```bash
npm install --save-dev jest
```

2. **Yksinkertaisen testin kirjoittaminen**:

Luo tiedosto nimeltä `sum.test.js`:

```javascript
const sum = require('./sum'); // Oletetaan, että tämä funktio yksinkertaisesti lisää kaksi numeroa

test('laskee 1 + 2 yhtä suureksi kuin 3', () => {
  expect(sum(1, 2)).toBe(3);
});
```

3. **Testisi ajaminen**:

```bash
npx jest
```

**Esimerkkituloste:**

```plaintext
PASS  ./sum.test.js
✓ laskee 1 + 2 yhtä suureksi kuin 3 (5ms)
```

### Asynkronisen koodin testaaminen

Jest tekee lupauksien ja async/await-syntaksin testaamisesta helppoa:

```javascript
// asyncSum.js
async function asyncSum(a, b) {
  return Promise.resolve(a + b);
}

// asyncSum.test.js
test('asynkroninen summaus toimii', async () => {
  await expect(asyncSum(1, 2)).resolves.toBe(3);
});
```

### Kolmansien osapuolien kirjastojen käyttö (Mocha & Chai)

Mocha on toinen suosittu testauskehys, jota usein käytetään yhdessä väittämäkirjasto Chain kanssa ilmaisuvoimaisempien testien tekemiseen.

1. **Asennus**:

```bash
npm install --save-dev mocha chai
```

2. **Testin kirjoittaminen Mochan ja Chain kanssa**:

Luo `calculate.test.js`:

```javascript
const chai = require('chai');
const expect = chai.expect;

const calculate = require('./calculate'); // Yksinkertainen laskentamoduuli

describe('Calculate', function() {
  it('should sum two values', function() {
    expect(calculate.sum(5, 2)).to.equal(7);
  });
});
```

3. **Testiesi ajaminen Mochalla**:

Lisää skripti `package.json`-tiedostoosi:

```json
"scripts": {
  "test": "mocha"
}
```

Sen jälkeen suorita:

```bash
npm test
```

**Esimerkkituloste:**

```plaintext
  Calculate
    ✓ should sum two values


  1 passing (8ms)
```

Nämä esimerkit havainnollistavat perus testien kirjoittamista ja suorittamista JavaScriptillä. Testauskehyksen, kuten Jestin tai Mochan Chain kanssa, käyttöönotto voi tarjota vankan perustan sovellustestaukselle, auttaen varmistamaan, että koodisi toimii tarkoitetulla tavalla päivitysten ja refaktorointien yli.
