---
title:                "Merkkijonon isoittaminen"
html_title:           "TypeScript: Merkkijonon isoittaminen"
simple_title:         "Merkkijonon isoittaminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi muuntaa merkkijonon isoiksi kirjaimiksi TypeScript-ohjelmassa? Yksinkertaisesti sanottuna se voi olla hyödyllistä, jos halutaan esimerkiksi korostaa tiettyä tekstiä tai tehdä merkkijono vertailukelpoiseksi muiden kanssa.

## Miten

```TypeScript
const capitalizeString = (str: string): string => {
  return str.toUpperCase();
}

console.log(capitalizeString("Tämä on esimerkki")); // TÄMÄ ON ESIMERKKI
```

Merkkijonon kääntäminen isoiksi kirjaimiksi TypeScript-ohjelmassa on helppoa! Ensin määritellään funktio, joka hyväksyy merkkijonon parametrina ja käyttää siihen sisäänrakennettua `toUpperCase()`-metodia, joka muuntaa kaikki kirjaimet isoiksi kirjaimiksi. Sitten kutsutaan funktiota halutulla merkkijonolla ja tulostetaan tulos konsoliin.

## Syvällisempi sukellus

Merkkijonon muuntaminen isoiksi kirjaimiksi TypeScript-ohjelmassa perustuu sisäänrakennettuun metodiin, joten sen käyttö on hyvin yksinkertaista. On kuitenkin huomioitava, että tämä metodi ei muuta alkuperäistä merkkijonoa, vaan palauttaa uuden muunnetun merkkijonon. 

Jos haluaisimme muuttaa kirjaimet alkuperäisessä merkkijonossa, voimme käyttää siihen `toUpperCase()`-metodin sijaan `toLowerCase()`-metodia, joka muuntaa kaikki kirjaimet pieniksi kirjaimikisi. 

Esimerkiksi, jos haluaisimme muuttaa vain ensimmäisen kirjaimen isoksi ja muut pieniksi, voisimme käyttää tätä koodia:

```TypeScript
const capitalizeString = (str: string): string => {
  const firstLetter = str.charAt(0).toUpperCase();
  const restOfStr = str.slice(1).toLowerCase();
  return firstLetter + restOfStr;
}

console.log(capitalizeString("tÄmÄ ON eSiMerkKi")); // Tämä on esimerkki
```

Tässä koodissa ensin määritellään muuttuja, joka saa arvon ensimmäisestä merkistä muunnettuna isoksi kirjaimeksi. Sitten määritellään toinen muuttuja, joka saa arvoksi alkuperäisen merkkijonon ensimmäisen kirjaimen jälkeiset merkit muunnettuna pieniksi kirjaimiksi. Lopuksi yhdistetään nämä kaksi muuttujaa ja palautetaan uusi merkkijono.

## Katso myös

- [MDN: String.toUpperCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [MDN: String.toLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)