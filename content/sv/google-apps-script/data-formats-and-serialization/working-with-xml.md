---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:41.589733-07:00
description: "Hur: Google Apps Script tillhandah\xE5ller `XmlService` f\xF6r att arbeta\
  \ med XML-data. Nedan demonstrerar vi hur man tolkar en XML-str\xE4ng, modifierar\
  \ dess\u2026"
lastmod: '2024-03-13T22:44:37.465432-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script tillhandah\xE5ller `XmlService` f\xF6r att arbeta med\
  \ XML-data."
title: Arbeta med XML
weight: 40
---

## Hur:
Google Apps Script tillhandahåller `XmlService` för att arbeta med XML-data. Nedan demonstrerar vi hur man tolkar en XML-sträng, modifierar dess innehåll och genererar en ny XML-sträng.

Att tolka en XML-sträng:

```javascript
function parseXML() {
  var xmlString = '<root><child name="first">Hello</child><child name="second">World</child></root>';
  var document = XmlService.parse(xmlString);
  var root = document.getRootElement();
  var children = root.getChildren('child');
  Logger.log(children[0].getText()); // Loggar: Hello
}
```

För att ändra XML kanske du vill lägga till ett nytt barn-element:

```javascript
function addNewChild() {
  var xmlString = '<root><child name="first">Hello</child></root>';
  var document = XmlService.parse(xmlString);
  var root = document.getRootElement();
  
  var newChild = XmlService.createElement('child').setText('World');
  root.addContent(newChild);
  
  var xml = XmlService.getPrettyFormat().format(document);
  Logger.log(xml);
  // Loggar den nya XML-strängen med det tillagda barn-elementet
}
```

Att generera XML-sträng från grunden:

```javascript
function createXML() {
  var root = XmlService.createElement('root');
  var child = XmlService.createElement('child').setText('Hello World');
  root.addContent(child);
  
  var xml = XmlService.getPrettyFormat().format(XmlService.createDocument(root));
  Logger.log(xml);
  // Utmatning: <root><child>Hello World</child></root>
}
```

## Fördjupning
Historiskt sett var XML (Extensible Markup Language) den facto standarden för datautbyte innan JSON dök upp som ett lättviktsalternativ. XML:s utförliga syntax och strikta tolkningsmodell tillhandahöll ett robust, om än klumpigt, dataformat. I Google Apps Script kapslar `XmlService` API:et in skapandet, tolkningen och manipuleringen av XML-data, och erkänner dess fortsatta betydelse i olika äldre och företagssystem, SOAP-webbtjänster och konfigurationsfiler för applikationer.

Trots JSON:s prevalens i modern webbutveckling för dess enkelhet och användarvänlighet med JavaScript, förblir XML relevant på områden där dokumentvalidering och strukturerade hierarkier är avgörande. Dock, för nya projekt, särskilt de som lutar mot webb-API:er, är JSON ofta det mer praktiska valet på grund av dess lättviktiga natur och sömlösa integration med JavaScript.

Att förstå XML och dess hantering i Google Apps Script är av största vikt för utvecklare som arbetar i miljöer där integration med äldre system eller specifika företags-API:er är nödvändig. Dock är det när man startar nya projekt eller när flexibilitet är nyckeln, rådligt att utvärdera behovet av XML över alternativ som JSON.
