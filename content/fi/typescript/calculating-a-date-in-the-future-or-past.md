---
title:                "Suunniteltaessa päivämäärää tulevaisuudessa tai menneisyydessä"
html_title:           "TypeScript: Suunniteltaessa päivämäärää tulevaisuudessa tai menneisyydessä"
simple_title:         "Suunniteltaessa päivämäärää tulevaisuudessa tai menneisyydessä"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Joskus saatat tarvita laskea päiviä tulevaisuudessa tai menneisyydessä. Ehkä juhlia haluat tietää, kuinka kauan aikaa sinun on odotettava, tai ehkä sisällyttää päiväyssovellus ja tarvitsee tietää, kuinka monta päivää se on laajentunut.

## Miten

Jos haluat laskea päivän tulevaisuudessa tai menneisyydessä TypeScriptissä, sinun on ensin tuotava `Date` kirjasto ja määritettävä haluamasi päivämäärä. Sen jälkeen voit käyttää seuraavia esimerkkejä nähdäksesi, kuinka se toimii.

```TypeScript
import {Date} from "date";

// Määritä haluttu päivämäärä
let date = new Date(2020, 10, 1);

// Laske 10 päivää eteenpäin
date.setDate(date.getDate() + 10);

// Laske 5 viikkoa taaksepäin
date.setDate(date.getDate() - (5*7));
```

Tulostaa seuraavat:

```
2020-11-11
2020-09-27
```

## Syvemmälle

Voit myös suorittaa monimutkaisempia laskelmia, kuten laskea kuukausia tai vuosia tulevaisuudessa tai menneisyydessä. Voit myös käyttää muita `Date` -funktiota, kuten `setMonth()` tai `getFullYear()`, saadaksesi tarkempia laskelmia. Muista myös, että päivämäärät lasketaan aina alkaen 0, joten sinun on muutettava kuukausien järjestystä.

```
## Katso myös

- [Päivämäärän dokumentaatio TypeScriptissä](https://www.typescriptlang.org/docs/handbook/dates-and-times.html)
- [Päivämäärien laskeminen tulevaisuudessa ja menneisyydessä JavaScriptissä](https://www.w3schools.com/js/js_dates.asp)