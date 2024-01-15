---
title:                "Päivämäärän muuttaminen merkkijonoksi."
html_title:           "TypeScript: Päivämäärän muuttaminen merkkijonoksi."
simple_title:         "Päivämäärän muuttaminen merkkijonoksi."
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

On monia tilanteita, joissa tarvitaan muuntaa päivämäärä merkkijonoksi TypeScript-ohjelmassa. Tämä voi olla tarpeellista esimerkiksi käsiteltäessä käyttäjän antamia tietoja tai tallentaessa tietoja tietokantaan.

## Miten

Muuntaaksesi päivämäärän merkkijonoksi TypeScript-ohjelmassa, voit käyttää Date-tyyppisen muuttujan toLocaleDateString()-metodia. Tämä metodi palauttaa päivämäärän ja ajan käyttäjän sijaintiin ja aikavyöhykkeeseen perustuen.

```TypeScript
let today = new Date();
let dateString = today.toLocaleDateString();
console.log(dateString); // esimerkkilähtö: "04/12/2021"
```

Voit myös käyttää toLocaleString()-metodia, joka palauttaa päivämäärän ja kellonajan.

```TypeScript
let dateAndTimeString = today.toLocaleString();
console.log(dateAndTimeString); // esimerkkilähtö: "4/12/2021, 10:25:49 AM"
```

## Syvällinen sukellus

Voit määrittää toLocaleString()-metodille parametreja, joiden avulla voit muokata palautettavan merkkijonon muotoa. Esimerkiksi voit määrittää sijainnin ja aikavyöhykkeen sijasta eri kielen ja kellonajan muodon.

```TypeScript
let options = { weekday: 'long', year: 'numeric', month: 'long', day: 'numeric' };
let germanDateString = today.toLocaleDateString('de-DE', options);
console.log(germanDateString); // esimerkkilähtö: "Montag, 12. April 2021"
```

Voit myös käyttää moment.js-kirjastoa mukautettujen päivämäärän muotoilujen toteuttamiseen.

## Katso myös

- [MDN: Date.prototype.toLocaleDateString()](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleDateString)
- [MDN: Date.prototype.toLocaleString()](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleString)
- [moment.js](https://momentjs.com/)