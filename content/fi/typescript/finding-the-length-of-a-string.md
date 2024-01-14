---
title:                "TypeScript: Merkkijonon pituuden löytäminen"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Miksi löytää merkkijonon pituus on hyödyllistä

Merkkijonojen pituuden löytäminen on tärkeää monessa ohjelmoinnin kontekstissa. Se voi auttaa tarkistamaan syötetyn tekstin oikeellisuuden ja tarjota tietoa tekstin sisällöstä. Se voi myös auttaa laskemaan merkkien määrää tietyssä viestissä tai dokumentissa.

## Kuinka löydät merkkijonon pituuden TypeScriptillä

Merkkijonon pituuden löytäminen TypeScriptillä on yksinkertaista käyttämällä `.length` -ominaisuutta. Esimerkiksi:

```TypeScript
let nimi = "Maija";
console.log(nimi.length); // Output: 5
```

Tässä koodissa käytämme `.length` -ominaisuutta muuttujassa `nimi` ja tulostamme sen pituuden konsoliin.

Voit myös käyttää `string`-tyyppisen muuttujan metodeita, kuten `.charAt()` ja `.substring()` löytääksesi merkkijonon pituuden. Esimerkiksi:

```TypeScript
let lause = "Tämä on esimerkkilause.";
console.log(lause.charAt(9)); // Output: e
console.log(lause.substring(8, 17)); // Output: esimerkki
```

Koodissa käytämme `.charAt()` ja `.substring()` -metodeita löytääksemme tietyn merkin ja osan merkkijonosta.

## Syvempää tietoa merkkijonon pituudesta

Merkkijonot ovat välttämättömiä tietokoneohjelmoinnissa ja useimmat kielet tarjoavat erilaisia työkaluja niiden käsittelyyn. Merkkijonon pituuden löytäminen voi auttaa meitä tarkistamaan syötetyn tekstin oikeellisuuden, kuten esimerkiksi salasanan, tai laskemaan merkkien määrän dokumentissa. Se voi myös auttaa meitä luomaan toiminnallisuuksia, kuten rajapintoja ja lomakkeita, joissa meidän täytyy tarkistaa tekstikenttien pituus tai rajoittaa niiden maksimipituutta. 

## Katso myös

- [Official TypeScript documentation on strings](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [W3Schools tutorial on finding the length of a string in TypeScript](https://www.w3schools.com/code/tryit.asp?filename=G76KACHEE1JE)

Kiitos lukemisesta! Toivottavasti tästä oli apua ja hyötyä!