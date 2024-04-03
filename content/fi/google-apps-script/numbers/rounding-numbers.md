---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:03:14.762723-07:00
description: "Py\xF6rist\xE4minen, perusk\xE4site tietokoneohjelmoinnissa, tarkoittaa\
  \ luvun s\xE4\xE4t\xE4mist\xE4 l\xE4himp\xE4\xE4n kokonaislukuun tai tiettyyn m\xE4\
  \xE4r\xE4\xE4n desimaalipaikkoja.\u2026"
lastmod: '2024-03-13T22:44:56.090851-06:00'
model: gpt-4-0125-preview
summary: "Py\xF6rist\xE4minen, perusk\xE4site tietokoneohjelmoinnissa, tarkoittaa\
  \ luvun s\xE4\xE4t\xE4mist\xE4 l\xE4himp\xE4\xE4n kokonaislukuun tai tiettyyn m\xE4\
  \xE4r\xE4\xE4n desimaalipaikkoja."
title: "Lukujen py\xF6rist\xE4minen"
weight: 13
---

## Kuinka:
Google Apps Script, ollessaan JavaScript-pohjainen kieli, tarjoaa standardimenetelmiä numeroiden pyöristämiseen. Tässä on yhteenveto kolmesta yleisesti käytetystä tekniikasta:

### Math.round()
Tämä funktio pyöristää luvun lähimpään kokonaislukuun.

```javascript
var number = 2.56;
var roundedNumber = Math.round(number); 
Logger.log(roundedNumber); // Tulostaa: 3
```

### Math.ceil()
Pyöristää luvun ylöspäin lähimpään kokonaislukuun.

```javascript
var number = 2.56;
var roundedUp = Math.ceil(number); 
Logger.log(roundedUp); // Tulostaa: 3
```

### Math.floor()
Päinvastoin, pyöristää luvun alaspäin lähimpään kokonaislukuun.

```javascript
var number = 2.56;
var roundedDown = Math.floor(number); 
Logger.log(roundedDown); // Tulostaa: 2
```

Tiettyjen desimaalipaikkojen osalta voit käyttää `.toFixed()`, joka itse asiassa palauttaa merkkijonon, tai hienovaraisemman lähestymistavan matemaattiseen pyöristämiseen:

```javascript
var number = 2.56789;
var fixedNumber = number.toFixed(2); 
Logger.log(fixedNumber); // Tulostaa: "2.57" (merkkijonona)

var preciseRound = Math.round(number * 100) / 100; 
Logger.log(preciseRound); // Tulostaa: 2.57
```

## Syväsukellus
Numeroiden pyöristäminen Google Apps Scriptissä ei eroa paljoakaan siitä, miten se tehdään muissa JavaScript-ympäristöissä. Kuitenkin pyöristysmenetelmien erojen ymmärtäminen ja liukulukuaritmetiikan mahdollisten ongelmien tiedostaminen on ratkaisevaa. Esimerkiksi tietokoneiden tapa esittää liukulukuja johtaa siihen, että kaikkia desimaalilukuja ei voida esittää täydellisellä tarkkuudella, mikä voi johtaa joskus odottamattomiin pyöristystuloksiin.

Historiallisesti JavaScript (ja laajennettuna Google Apps Script) käsittelee tämän noudattamalla IEEE 754 -standardia, jota monet muut ohjelmointikielet käyttävät liukulukuaritmetiikkaan. Tämä standardi määrittelee, miten numerot pyöristetään, varmistaen yhtenäisyyden eri alustojen ja kielten välillä.

Vaikka Google Apps Scriptissä suorat pyöristysmenetelmät ovat suoraviivaisia ja usein riittäviä, monimutkaiset tai suurta tarkkuutta vaativat sovellukset saattavat hyötyä kirjastoista, kuten decimal.js tai big.js, jotka on suunniteltu käsittelemään mielivaltaisen tarkkuuden aritmetiikkaa. Nämä voivat olla erityisen hyödyllisiä työskenneltäessä taloudellisten tai tieteellisten laskelmien parissa, joissa pyöristettyjen numeroiden tarkkuus on ensiarvoisen tärkeää.

Muista kuitenkin, että ulkoisten kirjastojen käyttöönotto Google Apps Scriptissä edellyttää niiden lataamista käsikirjoittimen kautta, mikä saattaa tuoda mukanaan riippuvuuksia tai vaikuttaa skriptisi suorituskykyyn sen käyttötavasta riippuen. Monissa tapauksissa sisäänrakennetut Math-menetelmät ovat täysin riittäviä, mutta niissä reuna-tapauksissa, jotka vaativat tarkkuutta äärimmilleen, tarvitaan standardikirjaston ulkopuolelle katsomista.
