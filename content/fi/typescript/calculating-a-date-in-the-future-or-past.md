---
title:                "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
html_title:           "TypeScript: Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
simple_title:         "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Lasketaan tulevaisuuden tai menneisyyden päivämäärä viittaamalla tiettyyn ajanjaksoon tänään. Tätä tarvitaan usein, jotta ohjelman toiminnot ja tiedot pysyvät ajan tasalla ja relevantteina.

## Näin se tehdään:

TypeScriptissä voimme käyttää JavaScriptin `Date` ja `setDate` -metodia laskea-man tulevaisuuden tai menneisyyden päivämäärä.

```TypeScript
let tanaan = new Date();
let viikonPaasta = new Date();

viikonPaasta.setDate(tanaan.getDate() + 7); // Viikon kuluttua

console.log(`Tänään on: ${tanaan.toISOString()}`);
console.log(`Viikon kuluttua on: ${viikonPaasta.toISOString()}`);
```

Tämä antaa meille seuraavan tulostuksen:

```Bash
Tänään on: 2021-12-06T14:30:00.000Z
Viikon kuluttua on: 2021-12-13T14:30:00.000Z
```

## Sukellus syvälle

Historiassa ohjelmoijat käyttivät perinteisesti matalan tason kieliä päivämäärien ja aikojen käsittelyyn. Se on yleensä monimutkaista, ja laajempien JS-yhteisöjen tullessa mukaan luotiin yksinkertaisempia, korkean tason käsitteitä, kuten `Date`.

Vaihtoehtoja on myös useita. Voit käyttää libraries kuten Moment.js tarjoama lisäominaisuuksia ja joustavuutta päivämäären ja ajan käsittelyssä. Sinun on kuitenkin harkittava suorituskykyä ja projektin koosta riippuen.

Päivämäärän laskemista tulevaisuudessa tai menneisyydessä käytetään laajalti monissa käyttötapauksissa. Esimerkkejä ovat tapahtumien ajoittaminen, tehtävien aikataulutus, aikaistetut push-ilmoitukset ja monet muut.

## Katso myös

1. [Mozilla Developer Network - Date](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/Date)
2. [Moment.js](https://momentjs.com/)
3. [TypeScript Handbook - Date](https://www.typescriptlang.org/docs/handbook/basic-types.html#date)