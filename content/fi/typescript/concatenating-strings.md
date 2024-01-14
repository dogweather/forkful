---
title:                "TypeScript: Merkkijonojen yhdistäminen"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Jotkut saattavat ihmetellä, miksi ohjelmoinnissa tarvitaan merkkijonojen yhdistämistä. Yksinkertaisesti sanottuna, merkkijonojen yhdistäminen tarkoittaa useamman merkkijonon yhdistämistä yhdeksi suureksi merkkijonoksi. Tämä voi olla hyödyllistä esimerkiksi tekstin muotoilussa tai tiedon tallentamisessa.

## Kuinka tehdä se

TypeScriptillä on useita tapoja yhdistää merkkijonoja. Yksi tapa on käyttää konkreettista yhdistämisoperaattoria "+" kahden merkkijonon välillä.

```TypeScript
let nimi = "Matti";
let tervehdys = "Hei " + nimi + "!";
console.log(tervehdys);
```

Tämä tulostaa konsoliin "Hei Matti!". Voit myös käyttää String-interpolointia, joka mahdollistaa muuttujien käyttämisen suoraan merkkijonojen sisällä.

```TypeScript
let luku = 5;
let tulos = `Kuusi plus ${luku} on ${6 + luku}`;
console.log(tulos);
```

Tämä tulostaa konsoliin "Kuusi plus 5 on 11".

## Syvempi sukellus

TypeScriptissä merkkijonojen yhdistäminen tehdään taustalla muuttujien muuntamisena String-tyyppisiksi. Tämän vuoksi on hyvä huomioida, että liiallinen merkkijonojen yhdistely voi hidastaa koodin suorituskykyä. Paras tapa toteuttaa merkkijonojen yhdistely on käyttää StringBuilder-luokkaa, joka pystyy tehokkaasti käsittelemään suuria määriä merkkijonoja.

## Katso myös

- [TypeScriptin virallinen dokumentaatio](https://www.typescriptlang.org/docs/)
- [StringBuilder-luokan käyttäminen merkkijonojen yhdistelyssä (eng)](https://www.tutorialsteacher.com/csharp/csharp-stringbuilder)
- [String-interpolointi TypeScriptissä (eng)](https://flaviocopes.com/typescript-string-interpolation/)