---
date: 2024-01-20 17:38:05.623499-07:00
description: "How to: \"Syv\xE4sukellus\": P\xE4iv\xE4m\xE4\xE4rien muunto merkkijonoksi\
  \ on ollut tarpeen, kun tiedot on haluttu esitt\xE4\xE4 k\xE4ytt\xE4j\xE4lle ymm\xE4\
  rrett\xE4v\xE4ss\xE4 muodossa tai\u2026"
lastmod: '2024-04-05T22:51:10.476188-06:00'
model: gpt-4-1106-preview
summary: "\"Syv\xE4sukellus\": P\xE4iv\xE4m\xE4\xE4rien muunto merkkijonoksi on ollut\
  \ tarpeen, kun tiedot on haluttu esitt\xE4\xE4 k\xE4ytt\xE4j\xE4lle ymm\xE4rrett\xE4\
  v\xE4ss\xE4 muodossa tai l\xE4hetett\xE4ess\xE4 p\xE4iv\xE4m\xE4\xE4ri\xE4 eri j\xE4\
  rjestelmien v\xE4lill\xE4. JavaScriptin `Date`-olio tarjoaa perustan, jonka p\xE4\
  \xE4lle TypeScript rakentuu. Vaihtoehtoina ovat mm. kirjastot kuten Moment.js tai\
  \ Date-fns, jotka tarjoavat lis\xE4ominaisuuksia ja formaatteja. TypeScriptiss\xE4\
  \ k\xE4ytet\xE4\xE4n yleens\xE4 JavaScriptin `Date`-oliota, koska TypeScript on\
  \ JavaScriptin superset ja kirjoitusten tyypitys tuo lis\xE4turvaa. Kun k\xE4yt\xE4\
  t `toLocaleString`-metodia, saat muunnettua p\xE4iv\xE4m\xE4\xE4r\xE4n helposti\
  \ alueelliseen esitysmuotoon. Muuttujia voit muuttaa antamalla erilaisia argumentteja,\
  \ kuten `locales` ja `options`. `toISOString` puolestaan luo yleisesti k\xE4ytetyn,\
  \ standardin UTC-aikaan perustuvan merkkijonon."
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
