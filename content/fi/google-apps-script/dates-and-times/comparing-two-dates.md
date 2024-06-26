---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:49:58.276564-07:00
description: "Miten: Google Apps Scriptiss\xE4 p\xE4iv\xE4m\xE4\xE4ri\xE4 verrataan\
  \ k\xE4ytt\xE4m\xE4ll\xE4 JavaScriptin Date-objekteja, mik\xE4 mahdollistaa standardimenetelmien\
  \ k\xE4yt\xF6n arvioitaessa,\u2026"
lastmod: '2024-03-13T22:44:56.111865-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Scriptiss\xE4 p\xE4iv\xE4m\xE4\xE4ri\xE4 verrataan k\xE4ytt\xE4\
  m\xE4ll\xE4 JavaScriptin Date-objekteja, mik\xE4 mahdollistaa standardimenetelmien\
  \ k\xE4yt\xF6n arvioitaessa, kumpi kahdesta p\xE4iv\xE4m\xE4\xE4r\xE4st\xE4 on aikaisempi,\
  \ my\xF6h\xE4isempi tai ovatko ne samat."
title: "Kahden p\xE4iv\xE4m\xE4\xE4r\xE4n vertailu"
weight: 27
---

## Miten:
Google Apps Scriptissä päivämääriä verrataan käyttämällä JavaScriptin Date-objekteja, mikä mahdollistaa standardimenetelmien käytön arvioitaessa, kumpi kahdesta päivämäärästä on aikaisempi, myöhäisempi tai ovatko ne samat. Tässä on peruslähestymistapa:

```javascript
function compareDates() {
  var date1 = new Date('2023-04-01T00:00:00');
  var date2 = new Date('2023-04-15T00:00:00');

  // Vertaa päivämääriä
  if (date1 < date2) {
    Logger.log('Päivämäärä1 on ennen päivämäärää2');
  } else if (date1 > date2) {
    Logger.log('Päivämäärä1 on päivämäärän2 jälkeen');
  } else {
    Logger.log('Molemmat päivämäärät ovat samat');
  }
}

// Esimerkkituloste:
// Päivämäärä1 on ennen päivämäärää2
```

Tarkempiin vertailuihin (kuten päivien määrään kahden päivämäärän välillä) voit vähentää toisesta päivämäärästä toisen, mikä palauttaa eron millisekunteina:

```javascript
function daysBetweenDates() {
  var date1 = new Date('2023-04-01');
  var date2 = new Date('2023-04-15');
  
  var ero = date2 - date1;
  
  var päivät = ero / (1000 * 60 * 60 * 24); // Muunna millisekunnit päiviksi
  Logger.log(päivät + ' päivää päivämäärien välillä');
}

// Esimerkkituloste:
// 14 päivää päivämäärien välillä
```

## Syväsukellus
Google Apps Script hyödyntää JavaScriptin Date-objektien perusperiaatteita päivämäärien vertailussa, mikä on ollut kielen perustavaa laatua oleva osa sen alusta lähtien. Millisekuntien käyttö vertailuarvona Unix Epochin (1. tammikuuta 1970) jälkeen tarjoaa suuren tarkkuusasteen päivämäärien erojen tai yhtäläisyyksien määrittämiseksi.

Vaikka tämä lähestymistapa on tehokas useimmissa Google Apps Scriptin sovellusalueissa, on syytä huomata, että päivämäärien käsittelyyn liittyvät toiminnot - kuten aikavyöhykkeiden korjaukset ja karkausvuosilaskelmat - voivat joskus johtaa sekaannukseen. Kehittäjät muista ohjelmointitaustoista (kuten Python, jossa `datetime` ja `dateutil` moduulit tarjoavat hienostuneemman käsittelyn päivämäärille) saattavat pitää JavaScriptin Date-objektia ominaisuuksiltaan puutteellisena.

Monimutkaisempaan päivämäärien käsittelyyn ja manipulointiin yksinkertaisten vertailujen ulkopuolella, kirjastot kuten `Moment.js` (jota voidaan edelleen käyttää Google Apps Scriptissä ulkoisten API:en kautta) tarjoavat rikkaan joukon toiminnallisuuksia, jotka käsittelevät näitä puutteita. Kuitenkin natiivi JavaScriptin Date-objekti jatkaa toimimista luotettavana työkaluna useimmissa päivämäärien vertailutehtävissä, erityisesti Google Apps Scriptin ja sen integraation yhteydessä Googlen sovelluskokonaisuuden kanssa.
