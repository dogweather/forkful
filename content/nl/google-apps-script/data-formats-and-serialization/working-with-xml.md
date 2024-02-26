---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:33.586368-07:00
description: "Werken met XML in Google Apps Script stelt programmeurs in staat om\
  \ XML-gegevens te parseren, te manipuleren en te genereren, essentieel voor webservices\u2026"
lastmod: '2024-02-25T18:49:47.752323-07:00'
model: gpt-4-0125-preview
summary: "Werken met XML in Google Apps Script stelt programmeurs in staat om XML-gegevens\
  \ te parseren, te manipuleren en te genereren, essentieel voor webservices\u2026"
title: Werken met XML
---

{{< edit_this_page >}}

## Wat & Waarom?

Werken met XML in Google Apps Script stelt programmeurs in staat om XML-gegevens te parseren, te manipuleren en te genereren, essentieel voor webservices en configuraties. Programmeurs kiezen deze aanpak om te integreren met legacy systemen, web scraping uit te voeren of te communiceren met talrijke API's die nog steeds vertrouwen op XML boven JSON voor gegevensuitwisseling.

## Hoe:

Google Apps Script biedt de `XmlService` om met XML-gegevens te werken. Hieronder demonstreren we hoe we een XML-string kunnen parseren, de inhoud ervan kunnen wijzigen en een nieuwe XML-string kunnen genereren.

Een XML-string parseren:

```javascript
function parseXML() {
  var xmlString = '<root><child name="first">Hello</child><child name="second">World</child></root>';
  var document = XmlService.parse(xmlString);
  var root = document.getRootElement();
  var children = root.getChildren('child');
  Logger.log(children[0].getText()); // Logt: Hello
}
```

Om de XML te wijzigen, wilt u wellicht een nieuw kind-element toevoegen:

```javascript
function addNewChild() {
  var xmlString = '<root><child name="first">Hello</child></root>';
  var document = XmlService.parse(xmlString);
  var root = document.getRootElement();
  
  var newChild = XmlService.createElement('child').setText('World');
  root.addContent(newChild);
  
  var xml = XmlService.getPrettyFormat().format(document);
  Logger.log(xml);
  // Logt de nieuwe XML-string met het toegevoegde kind-element
}
```

Een XML-string vanaf het begin genereren:

```javascript
function createXML() {
  var root = XmlService.createElement('root');
  var child = XmlService.createElement('child').setText('Hallo Wereld');
  root.addContent(child);
  
  var xml = XmlService.getPrettyFormat().format(XmlService.createDocument(root));
  Logger.log(xml);
  // Geeft uit: <root><child>Hallo Wereld</child></root>
}
```

## Diepgaande Duik

Historisch gezien was XML (Extensible Markup Language) de de facto standaard voor gegevensuitwisseling voordat JSON opkwam als een lichtgewicht alternatief. XML's uitgebreide syntax en strikte parsingmodel zorgde voor een robuuste, zij het omvangrijke, gegevensindeling. In Google Apps Script omvat de `XmlService` API het creëren, parseren en manipuleren van XML-gegevens, waarbij de voortdurende belangrijkheid ervan in verschillende legacy en enterprise systemen, SOAP-webservices en configuratiebestanden voor applicaties wordt erkend.

Ondanks de prevalentie van JSON in moderne webontwikkeling vanwege de eenvoud en het gemak van gebruik met JavaScript, blijft XML relevant op gebieden waar documentvalidatie en gestructureerde hiërarchieën cruciaal zijn. Echter, voor nieuwe projecten, vooral diegene die neigen naar web-API's, is JSON vaak de praktischere keuze vanwege zijn lichtgewicht aard en naadloze integratie met JavaScript.

Het begrijpen van XML en de behandeling ervan in Google Apps Script is van het grootste belang voor ontwikkelaars die werken in omgevingen waar integratie met oudere systemen of specifieke enterprise-API's noodzakelijk is. Echter, bij het starten van nieuwe projecten of wanneer flexibiliteit sleutel is, is het raadzaam om de behoefte aan XML tegenover alternatieven zoals JSON te evalueren.
