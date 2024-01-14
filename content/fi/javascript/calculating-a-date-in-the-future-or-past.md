---
title:    "Javascript: Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Koska aikojen laskeminen tulevaisuuteen tai menneisyyteen voi olla tarpeellista monissa ohjelmointiprojekteissa, kuten aikaperusteisissa tapahtumissa tai deadlinen asettamisessa.

## Kuinka

### Laske tulevaisuuden päivämäärä

Jos haluat laskea tietyn päivämäärän tulevaisuuteen, voit käyttää Javascriptin `Date`-oliota ja sen `setDate()`-metodia sekä `getFullYear()`-, `getMonth()`- ja `getDate()`-metodeja.

```
let tulevaisuus = new Date(); // Luo uuden päivämääräolion, joka sisältää nykyisen päivän ja ajan
tulevaisuus.setDate(tulevaisuus.getDate() + 7); // Lisää 7 päivää nykyiseen päivämäärään
let paivays = tulevaisuus.getFullYear() + "-" + (tulevaisuus.getMonth()+1) + "-" + tulevaisuus.getDate(); // Muotoile päivämäärä haluttuun muotoon
console.log(paivays); // Tulostaa esimerkiksi "2021-09-23"
```

### Laske menneisyyden päivämäärä

Menneisyyden päivämäärän laskeminen on samanlainen prosessi kuin tulevaisuuden päivämäärän laskeminen, mutta tällöin käytetään `setDate()`-metodin sijaan `getDate()`-metodin vastakohtaa `setDate()` ja vähennetään haluttu määrä päiviä nykyisestä päivämäärästä.

```
let menneisyys = new Date(); // Luo uuden päivämääräolion, joka sisältää nykyisen päivän ja ajan
menneisyys.setDate(menneisyys.getDate() - 14); // Vähentää 14 päivää nykyisestä päivämäärästä
let paivays = menneisyys.getFullYear() + "-" + (menneisyys.getMonth()+1) + "-" + menneisyys.getDate(); // Muotoile päivämäärä haluttuun muotoon
console.log(paivays); // Tulostaa esimerkiksi "2021-09-09"
```

## Syventävä tieto

Javascriptin `Date`-olio käyttää Unix-timestampia, joka tallentaa päivämäärät ja ajat millisekunteina kuluneesta ajasta 1. tammikuuta 1970 klo 00:00 UTC. Tämä mahdollistaa päivämäärien ja aikojen helpon laskemisen ja muokkaamisen eri aikavyöhykkeiden välillä.

Voit myös hyödyntää lisäosia, kuten Moment.js, tehdäksesi päivämäärien käsittelystä ja muotoilusta helpompaa.

## Katso myös

- [Javascript Date API](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/)
- [Unix timestamp](https://en.wikipedia.org/wiki/Unix_time)