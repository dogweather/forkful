---
title:                "TypeScript: Merkkijonon päästäminen isoksi"
simple_title:         "Merkkijonon päästäminen isoksi"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit muuttaa merkkijonon kirjainkoon ensimmäisen kirjaimen suureksi?

Joissakin tapauksissa ohjelmoinnin prosessissa on tarpeen muuttaa merkkijonon kirjainkoko ensimmäisestä kirjaimesta suureksi. Tämä voi johtua esimerkiksi sisäänkirjautumisjärjestelmän vaatimuksesta tai käyttäjän nimien muotoilun odotuksista. Jatka lukemista oppiaksesi, kuinka tämä voidaan tehdä TypeScriptillä.

## Miten

```typescript
// Luodaan muuttuja merkkijonolla
let nimi: string = "mikko";

// Muunna merkkijonon kirjainkoko suureksi
let isoNimi = nimi.charAt(0).toUpperCase() // "M"

// Yhdistä suuri kirjain ja alkuperäinen merkkijono
let uusiNimi = isoNimi + nimi.slice(1); // "Mikko"

// Tulostaa: "Mikko"
console.log(uusiNimi);
```

Ensimmäisessä vaiheessa luomme muuttujan "nimi" ja määrittelemme sen arvoksi "mikko" merkkijonon. Sitten käytämme "charAt" -funktiota valitaksemme ensimmäisen kirjaimen ja muuttamaan sen kirjainkoko suureksi "toUpperCase" -funktiolla. Lopuksi yhdistämme suuren kirjaimen ja alkuperäisen merkkijonon käyttämällä "slice" -funktiota ja tulostamme uuden nimen konsoliin.

## Syvemmälle

Merkkijonon kirjainkoon muuttaminen on vain yksi tapa manipuloida merkkijonoja TypeScriptissä. Myös muut funktiot, kuten "toLowerCase" ja "replace", voivat olla hyödyllisiä merkkijonojen muotoilussa. TypeScriptillä on myös sisäänrakennettu rajapinta "String", jolla on monia hyödyllisiä toimintoja merkkijonojen käsittelemiseen.

## Katso myös

- [TypeScriptin viralliset dokumentit merkkijonojen käsittelystä](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [MDN:n opas merkkijonon käsittelystä JavaScriptillä](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)