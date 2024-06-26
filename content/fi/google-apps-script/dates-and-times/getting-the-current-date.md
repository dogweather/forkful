---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:54:53.904090-07:00
description: "Kuinka: Google Apps Script, joka perustuu JavaScriptiin, tarjoaa suoraviivaisia\
  \ menetelmi\xE4 nykyisen p\xE4iv\xE4m\xE4\xE4r\xE4n hankkimiseksi. Voit k\xE4ytt\xE4\
  \xE4 `new\u2026"
lastmod: '2024-03-13T22:44:56.109595-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script, joka perustuu JavaScriptiin, tarjoaa suoraviivaisia\
  \ menetelmi\xE4 nykyisen p\xE4iv\xE4m\xE4\xE4r\xE4n hankkimiseksi."
title: "Nykyisen p\xE4iv\xE4m\xE4\xE4r\xE4n hankkiminen"
weight: 29
---

## Kuinka:
Google Apps Script, joka perustuu JavaScriptiin, tarjoaa suoraviivaisia menetelmiä nykyisen päivämäärän hankkimiseksi. Voit käyttää `new Date()`-rakentajaa luodaksesi uuden päivämääräobjektin, joka edustaa nykyistä päivämäärää ja aikaa. Näin voit manipuloida ja näyttää sen eri muodoissa.

```javascript
function showCurrentDate() {
  var currentDate = new Date();
  
  Logger.log(currentDate); // Kirjaa nykyisen päivämäärän ja ajan skriptin aikavyöhykkeellä
  
  // Näyttääkseen vain päivämäärän YYYY-MM-DD muodossa
  var dateString = currentDate.getFullYear() + '-' + 
                   (currentDate.getMonth() + 1).toString().padStart(2, '0') + '-' + 
                   currentDate.getDate().toString().padStart(2, '0');
  Logger.log(dateString); // Esimerkkitulostus: "2023-04-01"
  
  // Näyttämällä luettavammassa muodossa
  var options = { year: 'numeric', month: 'long', day: 'numeric', hour: '2-digit', minute: '2-digit', second: '2-digit', timeZoneName: 'short' };
  var readableDate = currentDate.toLocaleDateString('en-US', options) + ' ' + 
                     currentDate.toLocaleTimeString('en-US', options);
                     
  Logger.log(readableDate); // Esimerkkitulostus: "huhtikuu 1, 2023, 12:00:00 IP GMT+1"
}
```

Nämä katkelmat osoittavat, kuinka nykyistä päivämäärää ja aikaa voi tallentaa ja muotoilla, osoittaen monipuolisuutta erilaisiin ohjelmointitarpeisiin Google Apps Scriptillä.

## Syväsukellus
Ennen kuin JavaScript vakiintui `Date`-objektiin, ohjelmoijien piti manuaalisesti pitää kirjaa ajasta ja päivämäärästä vähemmän standardien ja hankalammin käsiteltävien keinojen kautta. Tämä sisälsi aikaleima-integerien käytön ja itsetehtyjen päivämääräfunktioiden, jotka vaihtelivat ohjelmointiympäristöstä toiseen, johtaen epäjohdonmukaisuuteen ja yhteensopivuusongelmiin.

`new Date()`-objektin esittely JavaScriptissä, ja laajennettuna Google Apps Scriptissä, standardisoi päivämäärä- ja aikaoperaatiot, tehden niistä intuitiivisempia ja vähentäen tarvittavan koodin määrää päivämäärään liittyvissä toiminnoissa. On huomionarvoista, että vaikka Google Apps Scriptin toteutus on kätevä ja riittävä moniin sovelluksiin Googlen tuotevalikoimassa, se ei välttämättä vastaa kaikkia skenaarioita, erityisesti niitä, jotka vaativat monimutkaista aikavyöhykeiden käsittelyä tai tarkkaa aikaleiman kirjaamista nopeatempoisissa ympäristöissä.

Tällaisissa edistyneissä käyttötarkoituksissa ohjelmoijat kääntyvät usein JavaScriptin kirjastojen kuten Moment.js tai date-fns puoleen. Vaikka Google Apps Script ei natiivisti tue näitä kirjastoja, kehittäjät voivat jäljitellä joitakin niiden toiminnallisuuksia käyttämällä saatavilla olevia JavaScriptin Date-metodeja tai käyttämällä ulkoisia kirjastoja HTML-palvelun tai Apps Scriptin URL Fetch -palvelun kautta. Näistä vaihtoehdoista huolimatta Google Apps Scriptin natiivien päivämäärä- ja aikatoimintojen yksinkertaisuus ja integraatio pysyvät useimpien Googlen ekosysteemin tehtävien ensisijaisena valintana.
