---
title:                "Att arbeta med XML"
aliases: - /sv/javascript/working-with-xml.md
date:                  2024-01-26T04:32:41.767956-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att arbeta med XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/working-with-xml.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att arbeta med XML innebär att tolka, manipulera och producera XML-innehåll med hjälp av kod. Programmerare gör detta eftersom XML är brett använd för konfigurationsfiler, datautbyte och webbtjänster på grund av dess läsbarhet för både människor och maskiner.

## Hur man gör:

Så här tolkar du XML:

```javascript
let parser = new DOMParser();
let xmlString = `<note>
                    <to>Användare</to>
                    <from>Författare</from>
                    <heading>Påminnelse</heading>
                    <body>Glöm inte mig denna helg!</body>
                 </note>`;

let xmlDoc = parser.parseFromString(xmlString, "application/xml");
console.log(xmlDoc.getElementsByTagName('to')[0].childNodes[0].nodeValue);
// Utdata: Användare
```

Och för att producera XML:

```javascript
let xmlDocument = document.implementation.createDocument('', '', null);
let noteElement = xmlDocument.createElement('note');
noteElement.appendChild(xmlDocument.createElement('to')).textContent = 'Användare';
xmlDocument.appendChild(noteElement);
let serializer = new XMLSerializer();
let xmlString = serializer.serializeToString(xmlDocument);
console.log(xmlString);
// Utdata: <note><to>Användare</to></note>
```

## Djupdykning

XML står för eXtensible Markup Language, ett dataformat som har funnits sedan slutet av 90-talet. Det definierar en uppsättning regler för kodning av dokument som både människor och maskiner kan läsa. Historiskt sett har XML fått genomslag för sin flexibilitet och strukturerade hierarki, vilket har gjort det till ett val för webbtjänster, såsom SOAP, och många konfigurationsfiler.

Alternativ till XML inkluderar JSON (JavaScript Object Notation), som har blivit populärt för dess enkelhet att använda med JavaScript och för att det är mindre tungrott. YAML är ett annat alternativ, värderat för att vara både användarvänligt och ett vanligt val för konfiguration.

XML implementeras i JavaScript med DOMParser- och XMLSerializer-gränssnitten. XML DOM (Document Object Model) tillåter navigering och redigering av XML-dokument precis som du skulle göra med HTML. Trots JSON:s framfart är förståelsen av XML avgörande, eftersom många äldre system och specifika branscher fortfarande förlitar sig på det för datautbyte.

## Se även

- MDN Web Docs (XML-tolkning): https://developer.mozilla.org/en-US/docs/Web/API/DOMParser
- W3Schools (XML DOM-handledning): https://www.w3schools.com/xml/dom_intro.asp
- "Vad är XML?": https://www.w3.org/XML/
