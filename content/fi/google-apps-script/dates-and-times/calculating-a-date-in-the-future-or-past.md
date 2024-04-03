---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:49:25.624898-07:00
description: "Kuinka tehd\xE4: Google Apps Scriptiss\xE4, joka perustuu JavaScriptiin,\
  \ voit manipuloida p\xE4iv\xE4m\xE4\xE4ri\xE4 k\xE4ytt\xE4m\xE4ll\xE4 `Date`-objektia.\
  \ N\xE4in voit laskea\u2026"
lastmod: '2024-03-13T22:44:56.113011-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Scriptiss\xE4, joka perustuu JavaScriptiin, voit manipuloida\
  \ p\xE4iv\xE4m\xE4\xE4ri\xE4 k\xE4ytt\xE4m\xE4ll\xE4 `Date`-objektia."
title: "Tulevaisuuden tai menneisyyden p\xE4iv\xE4m\xE4\xE4r\xE4n laskeminen"
weight: 26
---

## Kuinka tehdä:
Google Apps Scriptissä, joka perustuu JavaScriptiin, voit manipuloida päivämääriä käyttämällä `Date`-objektia. Näin voit laskea tulevaisuuden ja menneisyyden päivämääriä:

### Tulevaisuuden päivämäärän laskeminen
Tulevaisuuden päivämäärän laskemiseksi luot päivämääräobjektin nykyiselle päivämäärälle ja lisäät siihen halutun määrän päiviä (tai mitä tahansa muita aikayksiköitä).

```javascript
// Nykyinen päivämäärä
var today = new Date();

// Laske 10 päivän päästä oleva päivämäärä
var futureDate = new Date(today);
futureDate.setDate(today.getDate() + 10);

Logger.log("Tulevaisuuden päivämäärä: " + futureDate.toDateString());
```

### Menneisyyden päivämäärän laskeminen
Samoin menneisyydessä olevan päivämäärän löytämiseksi vähennät päivien määrän nykyisestä päivämäärästä.

```javascript
// Nykyinen päivämäärä
var today = new Date();

// Laske 10 päivää sitten ollut päivämäärä
var pastDate = new Date(today);
pastDate.setDate(today.getDate() - 10);

Logger.log("Menneisyyden päivämäärä: " + pastDate.toDateString());
```

### Esimerkkitulostus
Tämä tulostaisi jotakin seuraavasta (olettaen, että tänään on 15. huhtikuuta 2023):

```
Tulevaisuuden päivämäärä: Ti Huhti 25 2023
Menneisyyden päivämäärä: Ke Huhti 05 2023
```

Muista, että JavaScriptin Date-objekti (ja siten myös Google Apps Scriptissä) säätää automaattisesti kuukausia ja vuosia, kun lisäät tai vähennät päiviä.

## Syväsukellus
Päivämäärien manipulointi `Date`-objektin avulla juontaa juurensa alkuaikojen JavaScript-toteutuksiin. Ajan kuluessa tämä lähestymistapa on yleisesti pysynyt johdonmukaisena, tarjoten kehittäjille suoraviivaisen tavan hallita päivämääriä ilman ulkopuolisten kirjastojen tarvetta. Kuitenkin monimutkaisempia toimintoja, kuten aikavyöhykkeiden säätöjä, varten tai laajojen päivämääräpohjaisten tietojen kanssa työskenneltäessä, kirjastot kuten `Moment.js` tai modernimpi `Luxon` saattavat tarjota enemmän toiminnallisuuksia ja helpompaa käsittelyä.

Erityisesti Google Apps Scriptissä, huolimatta `Date`-objektin suorasta saatavuudesta ja yksinkertaisuudesta, on tärkeää olla tietoinen siitä, miten päivämäärälaskelmat voivat vaikuttaa skriptin suorituskykyyn ja suoritusaikaan, erityisesti aikaohjattujen laukaisijoiden tai laajojen taulukkolaskentojen manipuloinnissa. Lisäksi, vaikka Google Apps Script tarjoaa sisäänrakennettuja menetelmiä päivämäärien käsittelyyn sen ekosysteemin sisällä (kuten Google Sheets tai Calendar), ulkoisten kirjastojen integrointi tai Googlen Advanced Servicesin hyödyntäminen saattaa toisinaan tarjota kestävämpiä ratkaisuja monimutkaisiin skenaarioihin.

Näin ollen, vaikka alkuperäinen JavaScriptin `Date`-objektin metodologia on yleensä riittävä suoraviivaisten laskelmien tekemiseen, ulkoisten kirjastojen tai palvelujen tutkiminen saattaa parantaa toiminnallisuutta monimutkaisemmissa vaatimuksissa.
