---
aliases:
- /fi/google-apps-script/working-with-xml/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:48.441875-07:00
description: "XML:n k\xE4sittely Google Apps Scriptill\xE4 mahdollistaa ohjelmoijien\
  \ j\xE4sent\xE4\xE4, manipuloida ja luoda XML-dataa, mik\xE4 on olennaista verkkopalveluille\
  \ ja\u2026"
lastmod: 2024-02-18 23:09:07.164666
model: gpt-4-0125-preview
summary: "XML:n k\xE4sittely Google Apps Scriptill\xE4 mahdollistaa ohjelmoijien j\xE4\
  sent\xE4\xE4, manipuloida ja luoda XML-dataa, mik\xE4 on olennaista verkkopalveluille\
  \ ja\u2026"
title: "Ty\xF6skentely XML:n kanssa"
---

{{< edit_this_page >}}

## Mikä ja miksi?

XML:n käsittely Google Apps Scriptillä mahdollistaa ohjelmoijien jäsentää, manipuloida ja luoda XML-dataa, mikä on olennaista verkkopalveluille ja -asetuksille. Ohjelmoijat omaksuvat tämän lähestymistavan integroitumaan perintöjärjestelmiin, suorittamaan verkkoskrapausta tai kommunikoimaan useiden API:en kanssa, jotka edelleen nojaavat XML:ään JSONin sijaan datan vaihdossa.

## Kuinka:

Google Apps Script tarjoaa `XmlService`-palvelun työskentelyyn XML-datan kanssa. Alla näytämme, kuinka jäsentää XML-merkkijono, muokata sen sisältöä ja luoda uusi XML-merkkijono.

XML-merkkijonon jäsentäminen:

```javascript
function parseXML() {
  var xmlString = '<root><child name="first">Hello</child><child name="second">World</child></root>';
  var dokumentti = XmlService.parse(xmlString);
  var juuri = dokumentti.getRootElement();
  var lapset = juuri.getChildren('child');
  Logger.log(lapset[0].getText()); // Lokit: Hello
}
```

XML:n muokkaamiseksi saatat haluta lisätä uuden lapsielementin:

```javascript
function addNewChild() {
  var xmlString = '<root><child name="first">Hello</child></root>';
  var dokumentti = XmlService.parse(xmlString);
  var juuri = dokumentti.getRootElement();
  
  var uusiLapsi = XmlService.createElement('child').setText('World');
  juuri.addContent(uusiLapsi);
  
  var xml = XmlService.getPrettyFormat().format(dokumentti);
  Logger.log(xml);
  // Lokitaa uuden XML-merkkijonon, jossa on lisätty lapsielementti
}
```

XML-merkkijonon luominen alusta:

```javascript
function createXML() {
  var juuri = XmlService.createElement('root');
  var lapsi = XmlService.createElement('child').setText('Hello World');
  juuri.addContent(lapsi);
  
  var xml = XmlService.getPrettyFormat().format(XmlService.createDocument(juuri));
  Logger.log(xml);
  // Tulostaa: <root><child>Hello World</child></root>
}
```

## Syväsukellus

Historiallisesti XML (Extensible Markup Language) oli de facto -standardi datan vaihdoille ennen kuin JSON nousi esiin kevyempänä vaihtoehtona. XML:n verbosi syntaksi ja tiukka jäsentämismalli tarjosi robustin, vaikkakin kömpelön, datamuodon. Google Apps Scriptissä `XmlService`-API kapseloi XML-datan luomisen, jäsentämisen ja manipuloinnin, tunnustaen sen jatkuvan merkityksen erilaisissa perintö- ja yritysjärjestelmissä, SOAP-verkkopalveluissa ja sovellusten konfiguraatiotiedostoissa.

Huolimatta JSONin vallankumouksellisesta asemasta modernissa web-kehityksessä sen yksinkertaisuuden ja helpon käytettävyyden vuoksi JavaScriptin kanssa, XML säilyy relevanttina alueilla, joissa asiakirjojen validointi ja rakenteelliset hierarkiat ovat olennaisia. Uusissa projekteissa, erityisesti niissä, jotka kallistuvat web APIeihin, JSON on usein käytännöllisempi vaihtoehto sen kevyen luonteen ja saumattoman integraation ansiosta JavaScriptin kanssa.

Ymmärtäminen XML ja sen käsittely Google Apps Scriptissä on tärkeää kehittäjille, jotka työskentelevät ympäristöissä, joissa integraatio vanhempien järjestelmien tai tiettyjen yritys-APIen kanssa on tarpeellista. Kuitenkin, kun aloitetaan uusia projekteja tai kun joustavuus on avainasemassa, XML:n tarpeen arviointi vaihtoehtoisiin, kuten JSON, verrattuna on suositeltavaa.
