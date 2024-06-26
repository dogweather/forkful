---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:53:01.989144-07:00
description: "Miten: Google Apps Scriptiss\xE4, joka perustuu moderniin JavaScriptiin,\
  \ merkkijonon osien poiminnan voi suorittaa usealla eri tavalla, mukaan lukien\u2026"
lastmod: '2024-03-13T22:44:56.084611-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Scriptiss\xE4, joka perustuu moderniin JavaScriptiin, merkkijonon\
  \ osien poiminnan voi suorittaa usealla eri tavalla, mukaan lukien `substring()`,\
  \ `substr()` ja `slice()` -metodit."
title: Alimerkkijonojen erottaminen
weight: 6
---

## Miten:
Google Apps Scriptissä, joka perustuu moderniin JavaScriptiin, merkkijonon osien poiminnan voi suorittaa usealla eri tavalla, mukaan lukien `substring()`, `substr()` ja `slice()` -metodit. Jokaisella on omat vivahteensa, mutta kaikki palvelevat tarkoitusta poimia määritellyt merkit merkkijonosta.

```javascript
// Esimerkki käyttäen substring()-metodia
var str = "Hello, world!";
var result = str.substring(0, 5);
console.log(result); // Tuloste: Hello

// Esimerkki käyttäen substr()-metodia
var resultSubstr = str.substr(7, 5);
console.log(resultSubstr); // Tuloste: world

// Esimerkki käyttäen slice()-metodia
var resultSlice = str.slice(-6);
console.log(resultSlice); // Tuloste: world!
```

Jokainen metodi ottaa kaksi argumenttia: aloituspaikan ja, slice()-metodia lukuun ottamatta joka voi ottaa negatiivisia indeksejä aloittaakseen lopusta, lopetuspaikan tai poimittavien merkkien määrän. On huomionarvoista, että alkuperäinen merkkijono pysyy muuttumattomana näiden toimintojen jälkeen, sillä ne palauttavat uusia merkkijonoarvoja.

## Syväsukellus
Historiallisesti JavaScriptin metodit merkkijonojen osien poimintaan ovat aiheuttaneet sekaannusta niiden samankaltaisten nimien ja toiminnallisuuksien vuoksi. Kuitenkin, Google Apps Scriptissä ja modernissa JavaScriptissä, `substring()` ja `slice()` ovat useimmiten käytössä, `substr()`-metodia pidetään vanhentuneena. Tämä on tärkeää huomioida kirjoitettaessa tulevaisuudenkestävää koodia.

Pääero `substring()` ja `slice()` -metodien välillä on siinä, miten ne käsittelevät negatiivisia indeksejä; `substring()` käsittelee negatiiviset indeksit nollana, kun taas `slice()` voi ottaa negatiivisen indeksin aloittaakseen poiminnan merkkijonon lopusta. Tämä tekee `slice()`-metodista erityisen kätevän tapauksissa, joissa merkkijonon tarkkaa pituutta ei ehkä tunneta tai kun on tarpeen poimia lopusta.

Päätettäessä, mitä metodia käytetään merkkijonon osien poimintaan, valinta usein kulminoituu toiminnon erityisvaatimuksiin (esim. onko negatiivisten indeksien käsittely hyödyllistä) ja henkilökohtaisiin tai tiimin koodaustapoihin. Vaikka yhtä parasta käytäntöä ei olekaan, hienovaraisten erojen ja suorituskyvyn vaikutusten ymmärtäminen voi auttaa tekemään perustellun päätöksen.
