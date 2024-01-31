---
title:                "Å jobbe med XML"
date:                  2024-01-26T04:32:37.548223-07:00
model:                 gpt-4-0125-preview
simple_title:         "Å jobbe med XML"

category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/working-with-xml.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
Å jobbe med XML betyr å analysere, manipulere og produsere XML-innhold ved hjelp av kode. Programmerere gjør det fordi XML er mye brukt til konfigurasjonsfiler, datautveksling og webtjenester på grunn av dens menneskelesbare og maskin-tolkbare natur.

## Hvordan:

Slik analyserer du XML:

```javascript
let parser = new DOMParser();
let xmlString = `<note>
                    <to>Bruker</to>
                    <from>Forfatter</from>
                    <heading>Påminnelse</heading>
                    <body>Ikke glem meg denne helgen!</body>
                 </note>`;

let xmlDoc = parser.parseFromString(xmlString, "application/xml");
console.log(xmlDoc.getElementsByTagName('to')[0].childNodes[0].nodeValue);
// Utdata: Bruker
```

Og for å produsere XML:

```javascript
let xmlDocument = document.implementation.createDocument('', '', null);
let noteElement = xmlDocument.createElement('note');
noteElement.appendChild(xmlDocument.createElement('to')).textContent = 'Bruker';
xmlDocument.appendChild(noteElement);
let serializer = new XMLSerializer();
let xmlString = serializer.serializeToString(xmlDocument);
console.log(xmlString);
// Utdata: <note><to>Bruker</to></note>
```

## Dyp dykk

XML står for eXtensible Markup Language, et dataformat som har vært rundt siden slutten av 90-tallet. Det definerer et sett med regler for koding av dokumenter som både mennesker og maskiner kan lese. Historisk sett fikk XML oppmerksomhet for sin fleksibilitet og strukturerte hierarki, noe som gjorde det til et valg for webtjenester, som SOAP, og tallrike konfigurasjonsfiler.

Alternativer til XML inkluderer JSON (JavaScript Object Notation), som har blitt populært for sin enkelhet i bruk med JavaScript og mindre vekt. YAML er et annet alternativ, verdsatt for å være både menneskevennlig og et vanlig valg for konfigurasjon.

XML er implementert i JavaScript ved hjelp av DOMParser- og XMLSerializer-grensesnittene. XML DOM (Document Object Model) tillater navigering og redigering av XML-dokumenter på samme måte som du ville ha gjort med HTML. Til tross for JSONs oppstigning, er forståelsen av XML nøkkelen, ettersom mange eldre systemer og spesifikke industrier fortsatt stoler på det for datautveksling.

## Se også

- MDN Web Docs (XML Parsing): https://developer.mozilla.org/en-US/docs/Web/API/DOMParser
- W3Schools (XML DOM Tutorial): https://www.w3schools.com/xml/dom_intro.asp
- "Hva er XML?": https://www.w3.org/XML/
