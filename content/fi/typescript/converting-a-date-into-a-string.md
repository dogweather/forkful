---
title:    "TypeScript: Muunna päiväys merkkijonoksi."
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi muuttaa päivämäärä merkkijonoksi?

Monissa ohjelmointiprojekteissa saattaa olla tarve muuttaa päivämäärä erilaisiin muotoihin, kuten merkkijonoksi. Esimerkiksi käyttäjän syöttämä päivämäärä voi olla JavaScript Date -objektina ja sen käsitteleminen voi olla helpompaa, kun se on muutettu merkkijonoksi. TypeScriptin avulla tämä tehtävä on helppo ratkaista.

## Miten tehdä se?

Muuttaaksesi päivämäärän merkkijonoksi, voit käyttää Date -objektin `toString()` -metodia. Se palauttaa päivämäärän merkkijonona halutussa muodossa. Alla on esimerkki TypeScript-koodista ja sen tuloksesta:

```TypeScript
// Alustetaan Date -objekti tällä hetkellä
const date = new Date();

// Muutetaan päivämäärä merkkijonoksi
const dateString = date.toString();

// Tulostetaan merkkijonopäivämäärä
console.log(dateString);
```

Tuloste: Wed Aug 18 2021 15:20:00 GMT+0300 (Eastern European Summer Time)

Voit myös määrittää tarkemman muotoilun `toLocaleString()` -metodin avulla. Tämä metodi hyväksyy kaksi valinnainen parametria: käyttäjän kielivalinnan ja halutun muodon. Alla on esimerkki, jossa päivämäärä muutetaan suomalaiseen muotoon ja palautetaan kortti muodossa:

```TypeScript
// Alustetaan Date -objekti tällä hetkellä
const date = new Date();

// Määritetään suomalainen kieli
const language = 'fi-fi';

// Määritetään lyhyt päivämäärän muoto
const options = {
  year: 'numeric',
  month: 'short',
  day: 'numeric'
};

// Muutetaan päivämäärä merkkijonoksi
const dateString = date.toLocaleString(language, options);

// Tulostetaan merkkijonopäivämäärä
console.log(dateString);
```

Tuloste: 18. Elokuuta 2021

## Syvempi sukellus

Puhtaassa JavaScriptissä `toString()` -metodi palauttaa päivämäärän merkkijonona täydessä ISO 8601 -muodossa. Kuitenkin TypeScriptin `toString()` -metodilla on erilainen käyttäytyminen, koska se perii sen Vertible -rajapinnasta. Tästä syystä se voi palauttaa myös muunlaisia merkkijonopäivämääriä. On tärkeää huomata, että `toLocaleString()` -metodi on useimmissa tapauksissa tarkempi vaihtoehto, kun haluat määrittää päivämäärän muodon.

## Katso myös

- [MDN dokumentaatio Date -objektista](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TS-DOCS dokumentaatio Date -objektista](https://www.typescriptlang.org/docs/handbook/standard-library.html#date)