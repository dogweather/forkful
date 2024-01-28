---
title:                "Att arbeta med XML"
date:                  2024-01-26T04:36:30.238170-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att arbeta med XML"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/working-with-xml.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att arbeta med XML innebär att tolka, manipulera och skriva XML-data med programmering. Programmerare hanterar XML för att utbyta data mellan olika system, för konfigurationsfiler eller när de arbetar med standarder som SOAP som är beroende av XML.

## Hur man gör:
```TypeScript
import { parseString } from 'xml2js';

// Exempel på XML
const xml = `<note>
                <to>Användare</to>
                <from>Författare</from>
                <heading>Påminnelse</heading>
                <body>Glöm inte mötet!</body>
             </note>`;

// Tolka XML till JSON
parseString(xml, (err, result) => {
    if(err) throw err;
    console.log(result);
});

// Antag att tolkningen var framgångsrik, utdata kan se ut så här:
// { note:
//    { to: ['Användare'],
//      from: ['Författare'],
//      heading: ['Påminnelse'],
//      body: ['Glöm inte mötet!'] } 
}
```

## Fördjupning
XML, eller Extensible Markup Language, har funnits sedan slutet av 90-talet. Dess självbeskrivande natur och läsbara format gjorde det populärt tidigt för olika applikationer såsom RSS-flöden, konfigurationshantering, och även kontorsdokumentformat som Microsoft Office Open XML. Men, det är omständligt jämfört med JSON, och strömmen har vänt. JSON har fått strålkastarljuset för webbaserade API:er på grund av dess lättare vikt och inbyggda kompatibilitet med JavaScript.

Trots detta är XML inte dött. Det används i storskaliga företagssystem och för dokumentstandarder som inte har övergått till JSON. Verktyg som `xml2js` för TypeScript eller `lxml` i Python bevisar att det finns ett fortsatt behov av manipulation av XML i programmering.

TypeScript har inte inbyggt stöd för XML som det har för JSON. I stället arbetar du med bibliotek. `xml2js` är ett exempel. Det omvandlar XML till JSON, vilket gör datan lättare för JavaScript-gurus att arbeta med.

## Se även
- [MDN Web Docs om XML](https://developer.mozilla.org/en-US/docs/Web/XML/XML_introduction)
- [xml2js npm-paket](https://www.npmjs.com/package/xml2js)
- [W3Schools XML-guiden](https://www.w3schools.com/xml/)
