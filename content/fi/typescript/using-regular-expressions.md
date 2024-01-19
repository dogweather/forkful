---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Haskell: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Säännölliset lausekkeet eli regex, ovat työkaluja tekstien hakemiseen, korvaamiseen ja analysoimiseen. Ohjelmoijat käyttävät niitä, koska ne tekevät monimutkaisista tekstinkäsittelytehtävistä nopeita ja tehokkaita.

## Kuinka:

Tässä on esimerkki kuinka käyttää säännöllisen lausekkeen TypeScriptissä:

```TypeScript
let regex = /Hello World/g;
let txt = "Hello World! Hello World!";
let result = txt.match(regex);
console.log(result);
```
Tulos olisi:

```
[ 'Hello World', 'Hello World' ]
```

## Syvempi sukellus:

**Historiallinen konteksti:** Regex syntyi 1950-luvulla, ja sen jälkeen se on tullut kiinteäksi osaksi ohjelmointikieliä tehostaen merkkijonojen käsittelyä.

**Vaihtoehdot:** Regexin sijaan voidaan käyttää myös manuaalista tekstinkäsittelyä tai kirjastoja, kuten XRegExp. Mutta ne eivät tarjoa regexin tehokkuutta ja monipuolisuutta.

**Toteutustiedot:** Säännölliset lausekkeet syntaksin ja toiminnallisuuden yksityiskohdat voivat vaihdella eri ohjelmointikielissä, myös TypeScriptissä. Käyttöön tarvitaan tehokkuutta ja tarkkuutta.

## Katso myös:

1. [MDN dokumentit säännöllisistä lausekkeista](https://developer.mozilla.org/fi/docs/Web/JavaScript/Guide/Regular_Expressions)
2. [TypeScriptin virallinen dokumentaatio](https://www.typescriptlang.org/docs/)
3. [RegexOne, oppia regex interaktiivisesti](https://regexone.com/)