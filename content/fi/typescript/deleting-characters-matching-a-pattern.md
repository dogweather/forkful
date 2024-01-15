---
title:                "Mallille vastaavien merkkien poistaminen"
html_title:           "TypeScript: Mallille vastaavien merkkien poistaminen"
simple_title:         "Mallille vastaavien merkkien poistaminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi poistaa merkkejä, jotka vastaavat tietytä kaavaa? Tämä voi olla hyödyllistä, jos esimerkiksi haluat puhdistaa tekstiä tai muuttaa sen muotoa tietyllä tavalla.

## Näin teet sen

```TypeScript
const teksti: string = "Tämä on esimerkki tekstistä."
const uusiTeksti: string = teksti.replace(/[Ttä]+/g, "")
console.log(uusiTeksti)
```

Tämä koodi poistaa kaikki "t" ja "ä" kirjaimet tekstin "Tämä on esimerkki tekstistä" ja tulostaa uuden tekstin "mo on esimerkki eksist." Miten tämä toimii? Vaihdetaan [Ttä]+ ilmaisun mukaiset merkit tyhjään merkkijonoon. Voit käyttää tätä tekniikkaa myös muissa kaavoissa tai sanojen korvaamiseen.

## Syvemmällä

Jos haluat oppia lisää, miten regex-kaavat toimivat TypeScriptissä ja miten voit muuttaa niitä erilaisiin tarkoituksiin, kannattaa tutustua TypeScriptin viralliseen dokumentaatioon ja löytää muita esimerkkejä ja oppaita verkosta.

## Katso myös

- [TypeScript dokumentaatio](https://www.typescriptlang.org/docs/)
- [Regex-kaavojen perusopas](https://codeburst.io/javascript-learn-regular-expressions-for-beginners-bb610383e154)
- [Regex-mallintaja](https://regexr.com/)