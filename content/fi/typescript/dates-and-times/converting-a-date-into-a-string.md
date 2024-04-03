---
date: 2024-01-20 17:38:05.623499-07:00
description: "\"Mik\xE4 ja Miksi?\" Muunnamme p\xE4iv\xE4m\xE4\xE4r\xE4n merkkijonoksi\
  \ hallitaksemme sen esitysmuotoa. T\xE4m\xE4 tekee p\xE4iv\xE4m\xE4\xE4rien kanssa\
  \ ty\xF6skentelyst\xE4 helpompaa, kuten\u2026"
lastmod: '2024-03-13T22:44:56.326623-06:00'
model: gpt-4-1106-preview
summary: "\"Mik\xE4 ja Miksi."
title: "P\xE4iv\xE4m\xE4\xE4r\xE4n muuntaminen merkkijonoksi"
weight: 28
---

## How to:
"Kuinka tehdä se:"

```TypeScript
const date: Date = new Date();

// ISO-stringiksi (ISO 8601 -muotoinen päivämäärä ja aika UTC:ssä)
const isoString: string = date.toISOString();
console.log(isoString); // Esim. "2023-03-15T12:34:56.789Z"

// Paikallinen aikavyöhyke, lyhyt muoto
const shortLocaleString: string = date.toLocaleDateString('fi-FI');
console.log(shortLocaleString); // Esim. "15.3.2023"

// Paikallinen aikavyöhyke, kokonainen muoto
const longLocaleString: string = date.toLocaleString('fi-FI');
console.log(longLocaleString); // Esim. "15.3.2023 klo 14.34.56"

// Määritellyt optiot
const customString: string = date.toLocaleString('fi-FI', {
  weekday: 'long', 
  year: 'numeric', 
  month: 'long', 
  day: 'numeric'
});
console.log(customString); // Esim. "keskiviikkona 15. maaliskuuta 2023"
```

## Deep Dive
"Syväsukellus":

Päivämäärien muunto merkkijonoksi on ollut tarpeen, kun tiedot on haluttu esittää käyttäjälle ymmärrettävässä muodossa tai lähetettäessä päivämääriä eri järjestelmien välillä. JavaScriptin `Date`-olio tarjoaa perustan, jonka päälle TypeScript rakentuu. Vaihtoehtoina ovat mm. kirjastot kuten Moment.js tai Date-fns, jotka tarjoavat lisäominaisuuksia ja formaatteja. 

TypeScriptissä käytetään yleensä JavaScriptin `Date`-oliota, koska TypeScript on JavaScriptin superset ja kirjoitusten tyypitys tuo lisäturvaa. Kun käytät `toLocaleString`-metodia, saat muunnettua päivämäärän helposti alueelliseen esitysmuotoon. Muuttujia voit muuttaa antamalla erilaisia argumentteja, kuten `locales` ja `options`. `toISOString` puolestaan luo yleisesti käytetyn, standardin UTC-aikaan perustuvan merkkijonon.

## See Also
"Katso myös":

- MDN Web Docs Date reference: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- TypeScript official documentation: [https://www.typescriptlang.org/docs/](https://www.typescriptlang.org/docs/)
- Date-fns documentation: [https://date-fns.org/](https://date-fns.org/) 
- Moment.js home page: [https://momentjs.com/](https://momentjs.com/) 

Muista, että käytänteet ja työkalut kehittyvät, joten pidä taitosi ajantasalla tutustumalla uusiin resursseihin ja päivitettyyn dokumentaatioon.
