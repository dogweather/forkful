---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:13.288557-07:00
description: "Werken met XML betekent het parseren, manipuleren en produceren van\
  \ XML-inhoud met behulp van code. Programmeurs doen dit omdat XML veel gebruikt\
  \ wordt\u2026"
lastmod: 2024-02-19 22:05:10.313644
model: gpt-4-0125-preview
summary: "Werken met XML betekent het parseren, manipuleren en produceren van XML-inhoud\
  \ met behulp van code. Programmeurs doen dit omdat XML veel gebruikt wordt\u2026"
title: Werken met XML
---

{{< edit_this_page >}}

## Wat & Waarom?
Werken met XML betekent het parseren, manipuleren en produceren van XML-inhoud met behulp van code. Programmeurs doen dit omdat XML veel gebruikt wordt voor configuratiebestanden, gegevensuitwisseling en webservices vanwege de leesbaarheid voor mensen en de verwerkbaarheid door machines.

## Hoe:

Hier is hoe je XML parsed:

```javascript
let parser = new DOMParser();
let xmlString = `<note>
                    <to>Gebruiker</to>
                    <from>Auteur</from>
                    <heading>Herinnering</heading>
                    <body>Vergeet mij niet dit weekend!</body>
                 </note>`;

let xmlDoc = parser.parseFromString(xmlString, "application/xml");
console.log(xmlDoc.getElementsByTagName('to')[0].childNodes[0].nodeValue);
// Output: Gebruiker
```

En om XML te produceren:

```javascript
let xmlDocument = document.implementation.createDocument('', '', null);
let noteElement = xmlDocument.createElement('note');
noteElement.appendChild(xmlDocument.createElement('to')).textContent = 'Gebruiker';
xmlDocument.appendChild(noteElement);
let serializer = new XMLSerializer();
let xmlString = serializer.serializeToString(xmlDocument);
console.log(xmlString);
// Output: <note><to>Gebruiker</to></note>
```

## Diepere Duik

XML staat voor eXtensible Markup Language, een gegevensformaat dat er sinds de late jaren 90 is. Het definieert een reeks regels voor het coderen van documenten die zowel door mensen als door machines gelezen kunnen worden. Historisch gezien won XML aan tractie vanwege zijn flexibiliteit en gestructureerde hiërarchie, waardoor het een keuze werd voor webservices, zoals SOAP, en talrijke configuratiebestanden.

Alternatieven voor XML omvatten JSON (JavaScript Object Notation), dat populair is geworden vanwege het gebruiksgemak met JavaScript en zijn lichtere gewicht. YAML is een ander alternatief, gewaardeerd om zowel de gebruiksvriendelijkheid voor mensen als een veelgebruikte keuze voor configuratie.

XML wordt in JavaScript geïmplementeerd met de DOMParser- en XMLSerializer-interfaces. De XML DOM (Document Object Model) maakt navigatie en bewerking van XML-documenten mogelijk, net zoals je dat met HTML zou doen. Ondanks de opkomst van JSON, is het begrijpen van XML cruciaal, aangezien tal van legacy systemen en specifieke industrieën er nog steeds op vertrouwen voor gegevensuitwisseling.

## Zie Ook

- MDN Web Docs (XML Parsing): https://developer.mozilla.org/en-US/docs/Web/API/DOMParser
- W3Schools (XML DOM Tutorial): https://www.w3schools.com/xml/dom_intro.asp
- "Wat is XML?": https://www.w3.org/XML/
