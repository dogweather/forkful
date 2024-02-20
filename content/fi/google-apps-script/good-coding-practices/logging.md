---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:56:00.882655-07:00
description: "Ohjelmistokehityksess\xE4 lokitus tarkoittaa tapahtumien, virheiden\
  \ tai merkitt\xE4vien esiintymien tallentamista suoritusaikana. Ohjelmoijat tekev\xE4\
  t n\xE4in\u2026"
lastmod: 2024-02-19 22:05:15.019882
model: gpt-4-0125-preview
summary: "Ohjelmistokehityksess\xE4 lokitus tarkoittaa tapahtumien, virheiden tai\
  \ merkitt\xE4vien esiintymien tallentamista suoritusaikana. Ohjelmoijat tekev\xE4\
  t n\xE4in\u2026"
title: Lokiointi
---

{{< edit_this_page >}}

## Mikä ja miksi?

Ohjelmistokehityksessä lokitus tarkoittaa tapahtumien, virheiden tai merkittävien esiintymien tallentamista suoritusaikana. Ohjelmoijat tekevät näin virheiden jäljittämiseksi, suorituskyvyn seuraamiseksi ja operatiivisen datan kirjaamiseksi, mikä on keskeistä ohjelmiston ylläpidossa ja sen käyttäytymisen ymmärtämisessä tuotannossa.

## Miten:

Google Apps Scriptissä lokitusta voi suorittaa käyttämällä erilaisia menetelmiä, kuten `Logger`-luokkaa ja `console.log()`-toimintoa. Logger-luokka on perinteinen tapa, joka sopii yksinkertaiseen virheenkorjaukseen ja kehitystarkoituksiin. Viimeaikaisten päivitysten myötä `console.log()` tarjoaa enemmän joustavuutta ja integraatiota Stackdriver Lokitukseen, tarjoten vankemman ratkaisun Apps Scriptiesi valvontaan Google Cloud Platformilla.

**Loggerin käyttö:**

```javascript
function logSample() {
  Logger.log('Tämä on yksinkertainen lokiviesti');
  
  var value = 5;
  Logger.log('Arvo on: %s', value); // Merkkijonon muotoilu
}

// Lokin tarkastelu:
// 1. Suorita logSample-funktio.
// 2. Näkymä -> Lokit
```

**Esimerkki Loggerin tulosteesta:**

```
[22-04-20 10:00:00:000 PDT] Tämä on yksinkertainen lokiviesti
[22-04-20 10:00:00:001 PDT] Arvo on: 5
```

**console.log() -käyttö:**

```javascript
function consoleLogSample() {
  console.log('Tämä viesti menee Stackdriver Lokitukseen');
  const obj = {name: 'Jane', role: 'Developer'};
  console.info('Oliota lokitetaan:', obj);
}

// Lokit voidaan tarkastella Google Cloud Platform (GCP) konsolissa Stackdriver Lokituksen alla
```

**Esimerkki console.log() -tulosteesta:**

```
Tämä viesti menee Stackdriver Lokitukseen
Oliota lokitetaan: {name: "Jane", role: "Developer"}
```

Siirtymällä `console.log()`-toiminnon käyttöön monimutkaisemmissa sovelluksissa kehittäjät voivat tehokkaasti jäsentää ja analysoida lokeja käyttämällä GCP:n tarjoamia tehokkaita suodattimia ja työkaluja, mikä ei ole yhtä suoraviivaista perinteisen Logger-luokan kanssa.

## Syväsukellus:

Lokitus Google Apps Scriptissä on kehittynyt merkittävästi. Alun perin `Logger`-luokka oli ensisijainen menetelmä kehittäjille skriptiensä vianmääritykseen. Se on yksinkertainen ja riittävä perusskripteille, mutta se ei tarjoa modernien pilvisovellusten vaatimia ominaisuuksia, kuten lokien etsimistä tai lokitrendien analysointia ajan myötä.

`console.log()`-toiminnon esittely paikkasi tämän kuilun integroimalla Google Apps Script -lokituksen Google Cloudin Stackdriver Lokituksen (nykyään kutsutaan Operations Suite) kanssa, tarjoten keskitetyn alustan sovellusten lokitukseen, valvontaan ja vianmääritykseen. Tämä ei ainoastaan mahdollistanut lokitusta suuressa mittakaavassa, vaan myös avasi edistyneitä lokienhallinnan ominaisuuksia, kuten lokiin perustuvat metriikat, reaaliaikainen lokianalyysi ja integraation muihin Google Cloud -palveluihin.

Vaikka `Logger` edelleen toimii tarkoitukseensa nopeassa vianmäärityksessä ja pienempien skriptien lokituksessa, siirtyminen käyttämään `console.log()`-toimintoa heijastaa laajempaa siirtymää skaalautuvien, pilvinatiivien sovellusten kehittämiseen. Se korostaa Googlen sitoutumista tarjoamaan kehittäjille työkaluja, jotka vastaavat tämän päivän sovellusten monimutkaisuutta ja mittakaavaa. Uusien tulokkaiden tulisi kuitenkin olla tietoisia hieman jyrkemmästä oppimiskäyrästä ja tarpeesta perehtyä Google Cloud Platformin konsepteihin. Tästä huolimatta siirtyminen on edullinen kehittäjille, jotka haluavat hyödyntää pilvivalmiuksia täysimääräisesti. Tämä yhteensovittaminen pilvipalveluiden kanssa on osa laajempaa suuntausta ohjelmistokehityksessä, korostaen vankkojen, skaalautuvien lokitusmekanismien merkitystä pilvilaskennan aikakaudella.
