---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:49.162853-07:00
description: "\xC5 jobbe med XML i Google Apps Script lar programmerere parse, manipulere\
  \ og generere XML-data, som er essensielt for webtjenester og konfigurasjoner.\u2026"
lastmod: '2024-03-13T22:44:40.342503-06:00'
model: gpt-4-0125-preview
summary: "\xC5 jobbe med XML i Google Apps Script lar programmerere parse, manipulere\
  \ og generere XML-data, som er essensielt for webtjenester og konfigurasjoner.\u2026"
title: Arbeide med XML
weight: 40
---

## Hva & Hvorfor?

Å jobbe med XML i Google Apps Script lar programmerere parse, manipulere og generere XML-data, som er essensielt for webtjenester og konfigurasjoner. Programmerere adopterer denne tilnærmingen for å integrere med eldre systemer, utføre nettskraping eller kommunisere med tallrike API-er som fremdeles stoler på XML over JSON for datautveksling.

## Hvordan:

Google Apps Script tilbyr `XmlService` for å jobbe med XML-data. Nedenfor demonstrerer vi hvordan du parser en XML-streng, modifiserer innholdet og genererer en ny XML-streng.

Parsing av en XML-streng:

```javascript
function parseXML() {
  var xmlString = '<root><child name="first">Hello</child><child name="second">World</child></root>';
  var document = XmlService.parse(xmlString);
  var root = document.getRootElement();
  var children = root.getChildren('child');
  Logger.log(children[0].getText()); // Logger: Hello
}
```

For å modifisere XML-en, kan du ønske å legge til et nytt barneelement:

```javascript
function addNewChild() {
  var xmlString = '<root><child name="first">Hello</child></root>';
  var document = XmlService.parse(xmlString);
  var root = document.getRootElement();
  
  var newChild = XmlService.createElement('child').setText('World');
  root.addContent(newChild);
  
  var xml = XmlService.getPrettyFormat().format(document);
  Logger.log(xml);
  // Logger den nye XML-strengen med det lagt til barneelementet
}
```

Generere XML-streng fra bunnen av:

```javascript
function createXML() {
  var root = XmlService.createElement('root');
  var child = XmlService.createElement('child').setText('Hello World');
  root.addContent(child);
  
  var xml = XmlService.getPrettyFormat().format(XmlService.createDocument(root));
  Logger.log(xml);
  // Utganger: <root><child>Hello World</child></root>
}
```

## Dykk dypere

Historisk var XML (Extensible Markup Language) den de facto standarden for datautveksling før JSON dukket opp som et lettere alternativ. XMLs verbøse syntaks og strenge parsingmodell ga et robust, om enn tungvint, dataformat. I Google Apps Script inneholder `XmlService` API-et opprettelse, parsing og manipulering av XML-data, og anerkjenner dens fortsatte betydning i ulike eldre og foretakssystemer, SOAP webtjenester og konfigurasjonsfiler for applikasjoner.

Til tross for JSONs dominans i moderne webutvikling for dets enkelhet og brukervennlighet med JavaScript, forblir XML relevant på områder hvor dokumentvalidering og strukturerte hierarkier er avgjørende. Imidlertid, for nye prosjekter, spesielt de som lener seg mot web-APIer, er JSON ofte det mer praktiske valget på grunn av sin lette natur og sømløse integrasjon med JavaScript.

Å forstå XML og dens håndtering i Google Apps Script er avgjørende for utviklere som jobber i miljøer hvor integrasjon med eldre systemer eller spesifikke foretaks-API-er er nødvendig. Imidlertid, når man starter nye prosjekter eller når fleksibilitet er nøkkel, er det tilrådelig å vurdere behovet for XML over alternativer som JSON.
