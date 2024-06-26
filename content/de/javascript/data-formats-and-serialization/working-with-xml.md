---
date: 2024-01-26 04:32:30.144854-07:00
description: "Wie geht das: XML steht f\xFCr eXtensible Markup Language, ein Datenformat,\
  \ das es seit den sp\xE4ten 90er Jahren gibt. Es definiert einen Satz von Regeln\
  \ zur\u2026"
lastmod: '2024-04-05T22:51:08.827368-06:00'
model: gpt-4-0125-preview
summary: "XML steht f\xFCr eXtensible Markup Language, ein Datenformat, das es seit\
  \ den sp\xE4ten 90er Jahren gibt."
title: Arbeiten mit XML
weight: 40
---

## Wie geht das:
So parsen Sie XML:

```javascript
let parser = new DOMParser();
let xmlString = `<note>
                    <to>User</to>
                    <from>Author</from>
                    <heading>Reminder</heading>
                    <body>Vergiss mich dieses Wochenende nicht!</body>
                 </note>`;

let xmlDoc = parser.parseFromString(xmlString, "application/xml");
console.log(xmlDoc.getElementsByTagName('to')[0].childNodes[0].nodeValue);
// Ausgabe: User
```

Und so erzeugen Sie XML:

```javascript
let xmlDocument = document.implementation.createDocument('', '', null);
let noteElement = xmlDocument.createElement('note');
noteElement.appendChild(xmlDocument.createElement('to')).textContent = 'User';
xmlDocument.appendChild(noteElement);
let serializer = new XMLSerializer();
let xmlString = serializer.serializeToString(xmlDocument);
console.log(xmlString);
// Ausgabe: <note><to>User</to></note>
```

## Tiefergehend
XML steht für eXtensible Markup Language, ein Datenformat, das es seit den späten 90er Jahren gibt. Es definiert einen Satz von Regeln zur Kodierung von Dokumenten, die sowohl von Menschen als auch von Maschinen gelesen werden können. Historisch gesehen gewann XML an Bedeutung wegen seiner Flexibilität und strukturierten Hierarchie, was es zu einer Wahl für Webdienste, wie SOAP, und zahlreiche Konfigurationsdateien machte.

Alternativen zu XML umfassen JSON (JavaScript Object Notation), das wegen seiner Benutzerfreundlichkeit mit JavaScript und geringerem Gewicht beliebt geworden ist. YAML ist eine weitere Alternative, die sowohl für Menschen freundlich als auch eine gängige Wahl für Konfigurationen ist.

XML wird in JavaScript mit den Schnittstellen DOMParser und XMLSerializer implementiert. Das XML DOM (Document Object Model) ermöglicht es, XML-Dokumente genauso zu navigieren und zu bearbeiten, wie man es mit HTML tun würde. Trotz des Aufstiegs von JSON ist das Verständnis von XML entscheidend, da zahlreiche Legacy-Systeme und spezifische Branchen immer noch darauf für den Datenaustausch angewiesen sind.

## Siehe auch
- MDN Web Docs (XML Parsing): https://developer.mozilla.org/en-US/docs/Web/API/DOMParser
- W3Schools (XML DOM Tutorial): https://www.w3schools.com/xml/dom_intro.asp
- "Was ist XML?": https://www.w3.org/XML/
