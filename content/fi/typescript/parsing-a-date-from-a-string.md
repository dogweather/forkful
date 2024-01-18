---
title:                "Päivämäärän erottaminen merkkijonosta"
html_title:           "TypeScript: Päivämäärän erottaminen merkkijonosta"
simple_title:         "Päivämäärän erottaminen merkkijonosta"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Parsing päivämäärä merkkijonosta TypeScriptillä

## Mitä ja miksi?
Päivämäärän parsiminen merkkijonosta tarkoittaa päivämäärän tiedon poimimista ja kääntämistä merkkijonosta siihen muotoon, jota ohjelma voi käsitellä. Tämä on tärkeä tehtävä, sillä esimerkiksi käyttäjän antama päivämäärätieto tulee usein merkkijonona ja ohjelman on muutettava se päivämääräksi, jotta se voidaan käsitellä javertailla toisiinsa. Päivämäärän parsiminen merkkijonosta on siis tärkeä osa monien ohjelmien toimintaa.

## Näin teet sen:
Koodiesimerkit ja näytemuoto tulostuu `TypeScript...`-lohkoissa.

### Parsiminen päivämäärä merkkijonosta käyttäen sisäänrakennettua Date-oliota:
```TypeScript
const dateString = "06/30/2021";
const date = new Date(dateString);

//tulostaa tänään olevan päivämäärän etäisyyttä annetusta päivämäärästä millisekunteina
console.log(date.getTime());
```
**Tulostaa:**
1625017200000 // tänään etäisyyta annetusta päivämäärästä


## Syvärinen sukellus:
### Historiallinen tausta:
Päivämäärän parsiminen merkkijonosta on ollut tarpeellista jo vuosien ajan, sillä käyttäjän antama päivämäärätieto on usein merkkijonona, kun taas ohjelmat käsittelevät päivämäärää tietynä tietotyyppinä.

### Vaihtoehtoiset tavat:
Vaikka tässä artikkelissa keskitymmekin TypeScriptin sisäänrakennettuun Date-olioon, on olemassa myös muita tapoja parsia päivämäärä merkkijonosta. Yksi mahdollisuus on käyttää ulkoista kirjastoa, kuten Moment.js, joka tarjoaa monipuolisia päivämääränkäsittelytoimintoja.

### Toteutuksen yksityiskohdat:
Kun päivämäärä parsitaan merkkijonosta, se muutetaan ensin Date-tyyppiseksi ja tallennetaan muuttujaan. Tämän jälkeen voidaan käyttää Date-olion metodeja, kuten "getDay()" tai "getMonth()" saadaksemme halutut päivämäärätiedot ulos.

## Katso myös:
- [MDN: Parse a date from string in JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/parse)
- [Moment.js](https://momentjs.com/)