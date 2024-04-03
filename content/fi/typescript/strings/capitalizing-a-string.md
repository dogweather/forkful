---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:04.336516-07:00
description: "Merkkijonon alkukirjaimen suurentaminen tarkoittaa annetun merkkijonon\
  \ ensimm\xE4isen merkin muuttamista suuraakkoseksi, jos se on pienaakkosessa, usein\u2026"
lastmod: '2024-03-13T22:44:56.299106-06:00'
model: gpt-4-0125-preview
summary: "Merkkijonon alkukirjaimen suurentaminen tarkoittaa annetun merkkijonon ensimm\xE4\
  isen merkin muuttamista suuraakkoseksi, jos se on pienaakkosessa, usein j\xE4tt\xE4\
  en loput merkkijonosta muuttumattomaksi."
title: Merkkijonon muuttaminen isoiksi kirjaimiksi
weight: 2
---

## Mikä & Miksi?
Merkkijonon alkukirjaimen suurentaminen tarkoittaa annetun merkkijonon ensimmäisen merkin muuttamista suuraakkoseksi, jos se on pienaakkosessa, usein jättäen loput merkkijonosta muuttumattomaksi. Tätä toimenpidettä käytetään yleensä varmistamaan, että erisnimet tai lauseiden alut noudattavat kielioppisääntöjä tekstinkäsittelyssä, mikä tekee tulosteista ammattimaisia ja luettavissa.

## Kuinka:

TypeScript, ollessaan JavaScriptin yläjoukko, tarjoaa erilaisia menetelmiä merkkijonojen alkukirjaimen suurennukseen, alkaen puhtaista JavaScript-lähestymistavoista kolmansien osapuolien kirjastojen hyödyntämiseen monimutkaisemmissa tai tiettyjä käyttötapauksia varten.

**Puhtaan JavaScriptin lähestymistapa:**

```typescript
function capitalize(str: string): string {
  return str.charAt(0).toUpperCase() + str.slice(1);
}

// Esimerkkituloste:
console.log(capitalize('hello TypeScript!')); // 'Hello TypeScript!'
```

Tämä menetelmä on yksinkertainen ja se nojaa `charAt()`-metodiin ensimmäisen merkin saamiseksi merkkijonosta ja `toUpperCase()`-metodiin sen muuttamiseksi suuraakkoseksi. `slice(1)`-metodi hakee sitten lopun merkkijonosta, jättäen sen muuttumattomaksi.

**Käyttäen Lodash-kirjastoa:**

Projekteissa, jotka jo käyttävät [Lodash](https://lodash.com/)-kirjastoa, voit hyödyntää sen `_.capitalize`-funktiota saavuttaaksesi saman tuloksen vähemmällä rungolla.

Asenna ensin Lodash:

```bash
npm install lodash
```

Käytä sitten sitä TypeScript-tiedostossasi:

```typescript
import * as _ from 'lodash';

// Esimerkkituloste:
console.log(_.capitalize('hello TypeScript!')); // 'Hello typescript!'
```

Huom: Lodashin `_.capitalize`-metodi muuntaa lopun merkkijonon pienaakkosiksi, mikä ei välttämättä aina ole toivottua.

**Käyttäen säännöllistä lauseketta:**

Säännöllinen lauseke voi tarjota suppean tavan suurentaa merkkijonon ensimmäinen kirjain, erityisesti jos tarvitset suurentaa jokaisen sanan ensimmäisen kirjaimen merkkijonossa.

```typescript
function capitalizeWords(str: string): string {
  return str.replace(/\b\w/g, char => char.toUpperCase());
}

// Esimerkkituloste:
console.log(capitalizeWords('hello typescript world!')); // 'Hello Typescript World!'
```

Tämä menetelmä käyttää `replace()`-funktiota etsimään sanojen rajoja seuraavan kirjaimen tai numeron (`\b\w`), suurentaen jokaisen vastaavuuden. Se on erityisen kätevä otsikoille tai otsakkeille.
