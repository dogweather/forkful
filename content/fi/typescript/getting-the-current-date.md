---
title:                "Päivämäärän hakeminen"
html_title:           "TypeScript: Päivämäärän hakeminen"
simple_title:         "Päivämäärän hakeminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Mitä ja Miksi?
Päivämäärän hankkiminen on tärkeä osa ohjelmointia, koska se auttaa meitä näyttämään tai tallentamaan tietoa tiettynä aikana. Esimerkiksi verkkosivulla käyttäjälle halutaan ehkä näyttää nykyinen päivämäärä tai tallentaa nykyinen aikaleima kuvaamaan uuden viestin lähetysaikaa.
 
# Miten:
TypeScript:ssa voit käyttää `new Date()`-metodia saadaksesi nykyisen päivämäärän ja ajan. Tämä antaa meille `Date`-objektin, joka sisältää monia hyödyllisiä metodeja, kuten `getDate()`, `getMonth()` ja `getFullYear()`. Voit myös muotoilla päivämäärän haluamallasi tavalla käyttämällä `toLocaleString()`-metodia ja antamalla halutut kohdemuotoiluasetukset.
 
```
TypeScript:
 
const currentDate = new Date();
console.log(currentDate.toLocaleString('fi-FI'));
 
Output:
 
10.9.2021 11:20:30
```
 
# Syvemmälle:
Päivämäärän hankkiminen on ollut tärkeä osa ohjelmointia jo pitkään, ja monilla kielillä on omat sisäänrakennetut työkalunsa siihen. Jotkut kieliin sisältyvät vaihtoehdot ovat `System.DateTime` C#-kielissä ja `DateTime` Java-kielissä.
 
Voit myös käyttää kolmannen osapuolen kirjastoja, kuten Moment.js, helpottamaan päivämäärän käsittelyä ja muotoilua. 
 
Implementaatiot voivat vaihdella eri kielissä ja kirjastoissa, mutta yleensä ne perustuvat Unix Timeen, joka on tavallaan "päivämäärän ja ajan nolla". Tämä tarkoittaa käytännössä sitä, että jokainen päivämäärä ja aika tallennetaan määrätyksiä sekunteina kuluessa Unix Time -arvosta.
 
# Katso myös:
- [Mozilla Developer Network: Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/)