---
title:                "Nykyisen päivämäärän hankkiminen"
date:                  2024-01-20T15:15:20.285590-07:00
html_title:           "Bash: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"

category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
JavaScriptillä saa nykyisen päivämäärän `Date`-objektilla. Koodaajat tarvitsevat tätä usein: päivämäärien vertailuun, aikaleimojen luomiseen, ja käyttäjälle ajan näyttämiseen.

## How to: (Kuinka tehdä:)
```Javascript
// Luodaan uusi Date-objekti
const nyt = new Date();

// Tulostetaan koko päivämäärä ja aika
console.log(nyt.toString()); // "Wed Apr 05 2023 15:46:11 GMT+0300 (Eastern European Summer Time)"

// Tulostetaan vain päivämäärä
console.log(nyt.toDateString()); // "Wed Apr 05 2023"

// Tulostetaan vain aika
console.log(nyt.toTimeString()); // "15:46:11 GMT+0300 (Eastern European Summer Time)"

// Tulostetaan ISO-muodossa
console.log(nyt.toISOString()); // "2023-04-05T12:46:11.318Z"
```

## Deep Dive: (Syväsukellus)
`Date`-objekti tuli JavaScriptiin 1.0-versiossa vuonna 1995. Helppoa päivämäärän käsittelyä varten. Mutta `Date` on rajoittunut. Se ei ota kantaa aikavyöhykkeisiin kovin hyvin, ja kansainvälistä tukea voisi parantaa.

Vaihtoehtoja:
- `moment.js`: Kirjasto, joka tarjoaa joustavuutta ja lisäominaisuuksia, muttei ole enää yhtä suosiossa kuin ennen.
- `date-fns`: Modulaarinen, ja kätevä käyttää. Sisältää tuen aikavyöhykkeille.
- `Luxon`: Moderni kirjasto, joka on syntynyt `moment.js`:n heikkouksien pohjalta. Tuettu hyvin.

JavaScript-kielellä käsitellään päivämäärää millisekunteina, jotka ovat kuluneet 1. tammikuuta 1970 (UNIX-aika). Varmista, että selaimet ja palvelimet käyttävät samaa aikavyöhykettä tai käsittele niiden erot koodissa.

## See Also: (Katso Myös)
- MDN Web Docs Date: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- `moment.js`: https://momentjs.com/
- `date-fns` kirjasto: https://date-fns.org/
- `Luxon` dokumentaatio: https://moment.github.io/luxon/#/
