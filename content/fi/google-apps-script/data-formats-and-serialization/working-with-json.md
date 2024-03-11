---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:19.263131-07:00
description: "JSON, eli JavaScript Object Notation, on kevyt muoto datan tallentamiseen\
  \ ja siirt\xE4miseen, ihanteellinen palvelimen ja asiakkaan v\xE4liseen kommunikointiin\u2026"
lastmod: '2024-03-11T00:14:30.047795-06:00'
model: gpt-4-0125-preview
summary: "JSON, eli JavaScript Object Notation, on kevyt muoto datan tallentamiseen\
  \ ja siirt\xE4miseen, ihanteellinen palvelimen ja asiakkaan v\xE4liseen kommunikointiin\u2026"
title: "Ty\xF6skentely JSON:n kanssa"
---

{{< edit_this_page >}}

## Mikä & Miksi?

JSON, eli JavaScript Object Notation, on kevyt muoto datan tallentamiseen ja siirtämiseen, ihanteellinen palvelimen ja asiakkaan väliseen kommunikointiin sekä konfiguraatiotiedostoihin. Ohjelmoijat hyödyntävät sitä Google Apps Scriptillä saumattoman datan vaihdon mahdollistamiseen Googlen palveluiden (kuten Sheets, Docs, Drive) ja ulkopuolisten lähteiden välillä, sen ihmisen luettavan rakenteen ja helpon integroitavuuden JavaScript-pohjaisiin ympäristöihin ansiosta.

## Miten:

Google Apps Scriptillä JSONin käsittely on suoraviivaista, suurelta osin natiivin tuen ansiosta, jonka JavaScript tarjoaa JSONin jäsentämiseen ja merkkijonoksi muuttamiseen. Tässä joitakin yleisiä operaatioita:

**1. JSONin jäsentäminen**: Oletetaan, että haemme JSON merkkijonon verkkopalvelusta; sen jäsentäminen JavaScript-objektiksi on välttämätöntä datan käsittelyä varten.

```javascript
var jsonString = '{"name": "Sample Project", "version": "1.0.0"}';
var obj = JSON.parse(jsonString);
Logger.log(obj.name); // Tuloste: Sample Project
```

**2. JavaScript-objektien merkkijonoksi muuttaminen**: Päinvastoin, JavaScript-objektin muuttaminen JSON merkkijonoksi on hyödyllistä, kun meidän tarvitsee lähettää dataa Apps Scriptistä ulkoiseen palveluun.

```javascript
var projectData = {
  name: "Sample Project",
  version: "1.0.0"
};
var jsonString = JSON.stringify(projectData);
Logger.log(jsonString); // Tuloste: '{"name":"Sample Project","version":"1.0.0"}'
```

**3. Monimutkaisen datan käsittely**:
Monimutkaisempien datarakenteiden, kuten objektien taulukoiden, kanssa prosessi pysyy samana, esitellen JSONin joustavuutta datan esittämisessä.

```javascript
var projects = [
  {name: "Project 1", version: "1.0"},
  {name: "Project 2", version: "2.0"}
];
var jsonString = JSON.stringify(projects);
Logger.log(jsonString); // Tuloste: '[{"name":"Project 1","version":"1.0"},{"name":"Project 2","version":"2.0"}]'
```

## Syväsukellus

JSONin kaikkialla läsnäolo nykyaikaisissa web-sovelluksissa ei voi olla aliarvioitu, juurtuen sen yksinkertaisuuteen ja miten saumattomasti se integroituu JavaScriptiin, webin kieleen. Sen suunnittelu, inspiroituna JavaScript-objektiliteraaleista, joskin tiukempi, helpottaa sen nopeaa omaksumista. 2000-luvun alussa JSON kasvatti suosiotaan vaihtoehtona XML:lle AJAX-vetoisissa web-sovelluksissa, tarjoten kevyemmän ja vähemmän verbosen datainterchange-formaatin. Ottaen huomioon Google Apps Scriptin syvän integraation erilaisten Googlen APIen ja ulkoisten palveluiden kanssa, JSON toimii keskeisenä formaattina datan rakenteellistamisessa, siirtämisessä ja käsittelyssä näiden alustojen välillä.

Vaikka JSON hallitsee web-sovellusten maailmassa, vaihtoehtoisia dataformaatteja kuten YAML konfiguraatiotiedostoille tai Protobuf tehokkaampaa binääriserialisointia varten korkean suorituskyvyn ympäristöissä on olemassa. Kuitenkin JSONin tasapaino luettavuuden, helppokäyttöisyyden ja laajan tuen välillä ohjelmointikielissä ja työkaluissa vahvistaa sen aseman monien kehittäjien oletusvalintana sukeltaessaan Google Apps Scriptin maailmaan ja sen ulkopuolelle.
