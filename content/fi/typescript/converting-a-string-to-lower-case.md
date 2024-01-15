---
title:                "Merkkijonon muuntaminen pienaakkosiksi"
html_title:           "TypeScript: Merkkijonon muuntaminen pienaakkosiksi"
simple_title:         "Merkkijonon muuntaminen pienaakkosiksi"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Suurin osa ohjelmointitehtävistä vaatii tekstin muokkaamista ja käsittelyä. Yksi yleinen tehtävä on muuttaa merkkijono pieniksi kirjaimiksi. Tässä artikkelissa käsittelemme, miksi ja miten muuntaa merkkijono pieniksi kirjaimiksi TypeScript-kielellä.

## Miten tehdä

```typescript
const merkkijono = "TÄMÄ ON TEKSTIÄ";
console.log(merkkijono.toLowerCase());
```

Tässä esimerkissä käytämme `toLowerCase()` -metodia muuttaaksemme merkkijonon pieniksi kirjaimiksi. Tämän metodin avulla voimme muokata merkkijonoa haluamaamme muotoon. Lopputuloksena tulostamme konsoliin "tämä on tekstiä".

## Syvällisempi tarkastelu

Merkkijonon muuttaminen pieniksi kirjaimiksi on yksi yleisimmistä tehtävistä, jotka ohjelmoijat kohtaavat. Se on tärkeää esimerkiksi silloin, kun vertaillaan kahta merkkijonoa, mutta halutaan silti huomioida mahdolliset kirjoitusvirheet tai eri kirjainkoot.

Toinen tapa muuttaa merkkijono pieniksi kirjaimiksi on käyttää `toLowerCase()` -metodia yhdessä `toLocaleLowerCase()` -metodin kanssa. Tämä ottaa huomioon myös mahdolliset kielelliset erot esimerkiksi ääkkösten käsittelyssä.

```typescript
console.log("TÄMÄ ON TEKSTIÄ".toLocaleLowerCase());
//tämä on tekstiä
```

On myös tärkeää huomata, että tyhjät välilyönnit tai muut erikoismerkit eivät muutu pieniksi kirjaimiksi käyttäessä `toLowerCase()` -metodia, vaan ne säilyvät samanlaisina.

## Katso myös

- [MDN Web Docs: String.prototype.toLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [MDN Web Docs: String.prototype.toLocaleLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleLowerCase)
- [TypeScript: String operations](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#string-operations)