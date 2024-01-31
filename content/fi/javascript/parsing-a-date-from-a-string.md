---
title:                "Merkkijonosta päivämäärän jäsentäminen"
date:                  2024-01-20T15:37:12.377385-07:00
html_title:           "Bash: Merkkijonosta päivämäärän jäsentäminen"
simple_title:         "Merkkijonosta päivämäärän jäsentäminen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä ja Miksi?)
Päivämäärän jäsentäminen merkkijonosta muuttaa tekstin päiväysmuotoon, jota koodi ymmärtää. Ohjelmoijat tekevät sen, koska sovellukset tarvitsevat usein aikatietoja laskentaan, varmistukseen tai ajan näyttämiseen käyttäjille.

## How to: (Kuinka tehdä:)
```Javascript
// Esimerkki: Merkkijonon jäsentäminen Date-objektiksi
const dateString = "2023-04-25T15:30:00.000Z";
const parsedDate = new Date(dateString);

console.log(parsedDate.toString()); // Tulostaa: Tue Apr 25 2023 18:30:00 GMT+0300 (Itä-Euroopan kesäaika)

// Esimerkki: Päivämäärän jäsentäminen epästandardista muodosta
const finnishDateString = "25.4.2023 15:30";
const dateParts = finnishDateString.split(/[. :]/);
const year = parseInt(dateParts[2], 10);
const month = parseInt(dateParts[1], 10) - 1; // Kuukaudet alkavat nollasta
const day = parseInt(dateParts[0], 10);
const hours = parseInt(dateParts[3], 10);
const minutes = parseInt(dateParts[4], 10);
const finnishDate = new Date(year, month, day, hours, minutes);

console.log(finnishDate.toString()); // Tulostaa: Tue Apr 25 2023 15:30:00 GMT+0300 (Itä-Euroopan kesäaika)
```

## Deep Dive (Syväsukellus):
Jäsentäminen oli ennen monimutkaisempaa ennen `Date.parse()` ja `Date` konstruktorin standardisointia. Päivämäärän jäsentämiseen löytyy monia kirjastoja, kuten Moment.js tai Date-fns, jotka tarjoavat lisätoiminnallisuutta ja helpompaa kansainvälistä tukea.

JavaScript selvittää päivämäärämerkkijonon itse kun käytetään `new Date()`, mutta muotojen on oltava yhteensopivia ECMAScriptin määritelmien kanssa. Epästandardit formaatit vaativat manuaalista jäsentämistä, kuten yllä esimerkissä.

ISO 8601 on kansainvälinen standardi päivämäärille, ja se on JavaScriptin suosittelema muoto: `YYYY-MM-DDTHH:mm:ss.sssZ`. Arvot ilman aikavyöhykettä käsitellään aina UTC:nä eli koordinoituina yleisaikoina.

## See Also (Katso Myös):
- MDN Date Reference: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- Date-fns kirjasto: https://date-fns.org/
- Moment.js kirjasto: https://momentjs.com/docs/#/parsing/
- ISO 8601 standardin tiedot: https://www.iso.org/iso-8601-date-and-time-format.html
- Aikavyöhykkeet JavaScriptissä: https://www.iana.org/time-zones
