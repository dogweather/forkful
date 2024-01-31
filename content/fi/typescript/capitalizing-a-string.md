---
title:                "Merkkijonon muuttaminen isoiksi kirjaimiksi"
date:                  2024-01-19
html_title:           "Arduino: Merkkijonon muuttaminen isoiksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen isoiksi kirjaimiksi"

category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
"Capitalizing a String" tarkoittaa tekstin aloittamista isolla alkukirjaimella. Ohjelmoijat käyttävät sitä luodakseen johdonmukaisen ulkoasun käyttöliittymiin tai muotoillakseen dataa käyttäjäystävällisesti.

## How to:
```TypeScript
function capitalizeFirstLetter(str: string): string {
  return str.charAt(0).toUpperCase() + str.slice(1);
}

console.log(capitalizeFirstLetter('moi kaikki')); // Moi kaikki
console.log(capitalizeFirstLetter('typescript on hauska')); // Typescript on hauska
```

## Deep Dive
Alkukirjainten suurentaminen on peräisin painotuotteista, jossa tärkeät sanat alkavat usein isolla alkukirjaimella. Vaihtoehtoina on käyttää kirjastoja, kuten Lodash, mutta perusmetodiikoista riittää usein. TypeScriptissä, kuten JavaScriptissä, tämä toteutetaan yleensä yhdistämällä `toUpperCase()`-metodia ensimmäiseen kirjaimeen ja `slice()`-metodia loppustringiin.

## See Also
- MDN Web Docs: String.charAt() - https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/charAt
- String.prototype.toUpperCase() - https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase
- Lodash capitalize - https://lodash.com/docs/#capitalize
