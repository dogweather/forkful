---
date: 2024-01-26 04:32:50.043810-07:00
description: "Kuinka: N\xE4in j\xE4sent\xE4t XML:\xE4\xE4."
lastmod: '2024-03-13T22:44:56.974203-06:00'
model: gpt-4-0125-preview
summary: "N\xE4in j\xE4sent\xE4t XML:\xE4\xE4."
title: "XML:n k\xE4sittely"
weight: 40
---

## Kuinka:
Näin jäsentät XML:ää:

```javascript
let parser = new DOMParser();
let xmlString = `<note>
                    <to>Käyttäjä</to>
                    <from>Kirjoittaja</from>
                    <heading>Muistutus</heading>
                    <body>Älä unohda minua tänä viikonloppuna!</body>
                 </note>`;

let xmlDoc = parser.parseFromString(xmlString, "application/xml");
console.log(xmlDoc.getElementsByTagName('to')[0].childNodes[0].nodeValue);
// Tuloste: Käyttäjä
```

Ja näin tuotat XML:ää:

```javascript
let xmlDocument = document.implementation.createDocument('', '', null);
let noteElement = xmlDocument.createElement('note');
noteElement.appendChild(xmlDocument.createElement('to')).textContent = 'Käyttäjä';
xmlDocument.appendChild(noteElement);
let serializer = new XMLSerializer();
let xmlString = serializer.serializeToString(xmlDocument);
console.log(xmlString);
// Tuloste: <note><to>Käyttäjä</to></note>
```

## Syväsukellus
XML on lyhennys sanoista eXtensible Markup Language, tiedostoformaatti, joka on ollut olemassa 90-luvun lopulta lähtien. Se määrittelee joukon sääntöjä dokumenttien koodaamiselle siten, että sekä ihmiset että koneet voivat lukea ne. Historiallisesti XML on saavuttanut suosiota joustavuutensa ja rakenteellisen hierarkiansa ansiosta, tehden siitä valinnan webservices-palveluille, kuten SOAP, ja lukuisille konfiguraatiotiedostoille.

Vaihtoehtoja XML:lle ovat mm. JSON (JavaScript Object Notation), joka on kasvattanut suosiotaan käyttöhelppoutensa ja keveytensä vuoksi JavaScriptin kanssa. YAML on toinen vaihtoehto, jota arvostetaan sen ihmisystävällisyyden ja yleisyyden vuoksi konfiguraatiossa.

XML on toteutettu JavaScriptissä käyttäen DOMParser ja XMLSerializer -rajapintoja. XML DOM (Document Object Model) mahdollistaa XML-dokumenttien selaamisen ja muokkaamisen aivan kuten tekisit HTML:llä. Huolimatta JSON:n noususta, XML:n ymmärtäminen on avainasemassa, sillä lukuisat legacy-järjestelmät ja tietyt teollisuudenalat edelleen luottavat siihen tiedonvaihdossa.

## Katso myös
- MDN Web Docs (XML-jäsentäminen): https://developer.mozilla.org/en-US/docs/Web/API/DOMParser
- W3Schools (XML DOM -opas): https://www.w3schools.com/xml/dom_intro.asp
- "Mikä on XML?": https://www.w3.org/XML/
