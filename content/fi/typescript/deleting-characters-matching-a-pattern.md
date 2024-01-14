---
title:    "TypeScript: Mallia vastaavien merkkien poistaminen"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmointitilanteissa saattaa olla tarpeen poistaa merkkejä, jotka vastaavat tiettyä **mallia**. Tämä voi tapahtua esimerkiksi koodien puhdistuksessa tai tietojen käsittelyssä. Tässä blogikirjoituksessa käydään läpi, kuinka poistaa merkkejä, jotka vastaavat tietyntyyppistä mallia TypeScript-ohjelmoinnissa.

## Kuinka tehdä

[Regular expression](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions) eli tavallinen lauseke on työkalu, joka mahdollistaa merkkijonon hakuun ja muokkaukseen. Tämä on hyödyllinen työkalu, kun halutaan poistaa merkkejä, jotka täyttävät tietyn määritellyn mallin.

Esimerkiksi halutessamme poistaa kaikki numerot ja erikoismerkit merkkijonosta, voimme käyttää seuraavaa regular expression -koodia:

```TypeScript
const merkkijono = "1a2b3c4d";
const puhdistettuMerkkijono = merkkijono.replace(/[0-9!@#$%^&*()]+/g, "");
console.log(puhdistettuMerkkijono);
```

Tulostus:

```TypeScript
abcd
```

Mallissa `[0-9!@#$%^&*()+]` määritellään, mitkä merkit halutaan poistaa. Tämän perään laitetaan `+` merkki, joka tarkoittaa, että mallin täytyy toistua vähintään kerran. Lopuksi lisätään vielä `g` merkki, joka tarkoittaa, että haku suoritetaan koko merkkijonosta eikä vain ensimmäisestä löydetystä kohdasta.

## Syvällisempiä tietoja

Regular expressionin käyttö voi aluksi tuntua haastavalta, mutta sen avulla voi saavuttaa paljon hyötyä. [Tässä](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions) on lisätietoa regular expressioneista ja niiden käyttämisestä JavaScriptissä, joka on TypeScriptin perusta.

## Katso myös

- [Regular expressioneiden käyttö JavaScriptissä](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [TypeScriptin virallinen dokumentaatio](https://www.typescriptlang.org/docs/)