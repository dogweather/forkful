---
title:                "Arbeiten mit XML"
aliases:
- de/google-apps-script/working-with-xml.md
date:                  2024-02-01T22:06:31.274691-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit XML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/google-apps-script/working-with-xml.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Die Arbeit mit XML in Google Apps Script ermöglicht es Programmierern, XML-Daten zu parsen, zu manipulieren und zu generieren, was für Webdienste und Konfigurationen unerlässlich ist. Programmierer wählen diesen Ansatz, um sich in ältere Systeme zu integrieren, Web-Scraping durchzuführen oder mit zahlreichen APIs zu kommunizieren, die noch auf XML anstelle von JSON für den Datenaustausch setzen.

## Wie:

Google Apps Script stellt den `XmlService` zur Verfügung, um mit XML-Daten zu arbeiten. Nachfolgend zeigen wir, wie man eine XML-Zeichenkette parst, deren Inhalt ändert und eine neue XML-Zeichenkette generiert.

Parsen einer XML-Zeichenkette:

```javascript
function parseXML() {
  var xmlString = '<root><child name="first">Hallo</child><child name="second">Welt</child></root>';
  var document = XmlService.parse(xmlString);
  var root = document.getRootElement();
  var children = root.getChildren('child');
  Logger.log(children[0].getText()); // Protokolliert: Hallo
}
```

Um das XML zu modifizieren, möchten Sie möglicherweise ein neues Kind-Element hinzufügen:

```javascript
function addNewChild() {
  var xmlString = '<root><child name="first">Hallo</child></root>';
  var document = XmlService.parse(xmlString);
  var root = document.getRootElement();
  
  var newChild = XmlService.createElement('child').setText('Welt');
  root.addContent(newChild);
  
  var xml = XmlService.getPrettyFormat().format(document);
  Logger.log(xml);
  // Protokolliert die neue XML-Zeichenkette mit dem hinzugefügten Kind-Element
}
```

Generierung einer XML-Zeichenkette von Grund auf:

```javascript
function createXML() {
  var root = XmlService.createElement('root');
  var child = XmlService.createElement('child').setText('Hallo Welt');
  root.addContent(child);
  
  var xml = XmlService.getPrettyFormat().format(XmlService.createDocument(root));
  Logger.log(xml);
  // Gibt aus: <root><child>Hallo Welt</child></root>
}
```

## Tiefergehende Betrachtung

Historisch gesehen war XML (Extensible Markup Language) vor dem Aufkommen von JSON als leichtgewichtige Alternative der de facto Standard für den Datenaustausch. XMLs ausführliche Syntax und striktes Parsing-Modell boten ein robustes, wenn auch sperriges, Datenformat. In Google Apps Script kapselt die `XmlService` API die Erstellung, das Parsen und die Manipulation von XML-Daten ein und erkennt deren fortgesetzte Bedeutung in verschiedenen Altsystemen und Unternehmensumgebungen, SOAP-Webdiensten und Konfigurationsdateien für Anwendungen an.

Trotz der Vorherrschaft von JSON in der modernen Webentwicklung wegen seiner Einfachheit und leichten Handhabung mit JavaScript, bleibt XML in Bereichen, in denen Dokumentenvalidierung und strukturierte Hierarchien entscheidend sind, relevant. Für neue Projekte, insbesondere solche, die sich auf Web-APIs konzentrieren, ist JSON jedoch oft die praktischere Wahl aufgrund seiner Leichtgewichtigkeit und nahtlosen Integration mit JavaScript.

Das Verständnis von XML und dessen Handhabung in Google Apps Script ist von größter Bedeutung für Entwickler, die in Umgebungen arbeiten, in denen eine Integration mit älteren Systemen oder bestimmten Unternehmens-APIs notwendig ist. Jedoch ist bei der Initiierung neuer Projekte oder wenn Flexibilität im Vordergrund steht, die Bewertung der Notwendigkeit von XML gegenüber Alternativen wie JSON ratsam.
