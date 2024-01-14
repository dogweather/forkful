---
title:    "TypeScript: Merkkijonon muuttaminen pieniksi kirjaimiksi"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Miksi

Joskus ohjelmointitehtävissä on tarpeen muuttaa merkkijono pienen kirjaimen muotoon. Tämä voi olla hyödyllistä esimerkiksi tietokantojen kanssa työskennellessä, jossa merkkijonon kirjainkoko ei välttämättä ole tärkeä.

## Miten

```TypeScript
const esimerkkiMerkkijono: string = "TÄmÄ on eSimerkki Tekstistä";

const pieniKirjain: string = esimerkkiMerkkijono.toLowerCase();

console.log(pieniKirjain); // tulostaa: "tämä on esimerkki tekstistä"
```

Tässä esimerkissä käytämme `toLowerCase()`-metodia, joka muuttaa kaikki merkkijonon kirjaimet pieniksi. Tämä metodi on luotu TypeScriptissa erityisesti tätä tarkoitusta varten.

## Syvemmälle

Vaikka `toLowerCase()`-metodi saattaa vaikuttaa yksinkertaiselta, on hyödyllistä tietää miten se toimii hieman tarkemmin. Metodi käsittelee merkkijonoa Unicode-standardin mukaisesti, joten se pystyy käsittelemään myös erikoismerkkejä ja kirjaimia, jotka eivät ole perinteisiä latinalaisia kirjaimia.

Jotta metodi toimisi oikein, tulee merkkijonon olla muunnettavissa muotoon `string`. Jos merkkijono on esimerkiksi muuttuja, tulee sen arvon olla ensin muutettu muotoon `string`.

## Katso myös

- [MDN - String.prototype.toLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [TypeScript Handbook - String Operations](http://www.typescriptlang.org/docs/handbook/basic-types.html#string-operations)
- [Unicode - Latin-1 Supplement Block](https://unicode-table.com/en/blocks/latin-1-supplement/)