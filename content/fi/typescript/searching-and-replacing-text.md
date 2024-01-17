---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "TypeScript: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

##Mitä & Miksi?

Tekstin etsiminen ja korvaaminen on prosessi, jossa ohjelmoija etsii tiettyjä merkkijonoja ja korvaa ne toisilla. Tämä voi olla hyödyllistä esimerkiksi, kun halutaan muuttaa tietynlaisia merkkijonoja toisenlaisiksi tai korvata vanhoja koodinpätkiä uusilla. Ohjelmoijat tekevät tätä parantaakseen koodin suorituskykyä ja tehokkuutta, sekä välttääkseen virheitä koodissa.

##Miten:

```TypeScript
const teksti = "Tämä on esimerkki tekstistä";
const uusiTeksti = teksti.replace("esimerkki", "toinen");

console.log(uusiTeksti);
//Tämä on toinen tekstistä
```

Tässä yksinkertaisessa esimerkissä käytetään replace-metodia, joka etsii merkkijonosta "teksti" sanaa "esimerkki" ja korvaa sen sanalla "toinen". Lopputulos tulostetaan konsoliin. Tämä on vain yksi tapa, jolla tekstiä voidaan etsiä ja korvata TypeScriptillä.

##Syvällinen sukellus:

Historiallisesti tekstiä on etsitty ja korvattu manuaalisesti, mikä on ollut aikaa vievää ja altistanut mahdollisille virheille. Nykyään tähän käytetään yleisesti ohjelmointikieliä ja kirjastoja, kuten TypeScript ja sen sisäänrakennettuja metodeja, kuten replace. On myös muita tapoja, kuten säännölliset lausekkeet (regular expressions), jotka tarjoavat laajempia etsimisen ja korvaamisen mahdollisuuksia.

##Katso myös:

Tässä artikkelissa olemme käsitelleet vain yhtä tapaa käyttää TypeScriptia tekstiä etsiessä ja korvatessa. On tärkeää tutustua myös muihin vaihtoehtoihin ja löytää itselle sopivin tapa tehdä tämä prosessi. Lisätietoa ja esimerkkejä löytyy esimerkiksi TypeScriptin virallisesta dokumentaatiosta (https://www.typescriptlang.org/docs/handbook/basic-types.html#string-handbook-regex) sekä eri ohjelmointifoorumeilta.