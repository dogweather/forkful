---
title:                "Nykyisen päivämäärän hankkiminen"
date:                  2024-01-20T15:16:59.204376-07:00
simple_title:         "Nykyisen päivämäärän hankkiminen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (Mikä ja Miksi?)
Koodaajat hakevat nykyistä päivämäärää usein, koska se on tarpeen aikaleimoihin, raportteihin ja käyttäjäkokemuksen räätälöintiin.

## How to: (Kuinka tehdä:)
```typescript
// Nykyisen päivämäärän hankkiminen TypeScriptillä
const currentDate = new Date();
console.log(currentDate.toISOString()); // Esimerkkitulostus: 2023-03-19T12:45:30.000Z
```

## Deep Dive (Sukellus syvemmälle)
### Historiakonteksti
JavaScript kehittyi 90-luvulla, ja `Date`-olio oli yksi sen alkuperäisistä API-osista. TypeScriptillä, joka on JavaScriptin superset, pääsy päivämääriin on säilynyt samankaltaisena.

### Vaihtoehdot
JavaScriptin `Date`-olio tarjoaa perustoiminnot, mutta kirjastot kuten `moment.js` tai `date-fns` tarjoavat laajempia toimintoja ja formaatteja. TypeScriptin tyypitys auttaa pitämään päivämääräkäsittelyn virheet kurissa.

### Toteutustiedot
`new Date()` luo uuden `Date`-olion, joka sisältää nykyisen hetken. `toISOString()`-metodi muuntaa tämän standardi ISO 8601 -muotoon, mutta muitakin formaatteja ja manipulointitapoja on.

## See Also (Katso myös)
- MDN Web Docs, Date: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- Date-fns, moderni JavaScript-kirjasto päivämääriin: https://date-fns.org/
- Moment.js, toinen suosittu päivämääräkirjasto: https://momentjs.com/
