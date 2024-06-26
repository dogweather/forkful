---
date: 2024-01-26 04:36:30.238170-07:00
description: "Hur man g\xF6r: XML, eller Extensible Markup Language, har funnits sedan\
  \ slutet av 90-talet. Dess sj\xE4lvbeskrivande natur och l\xE4sbara format gjorde\
  \ det\u2026"
lastmod: '2024-04-05T21:53:39.008812-06:00'
model: gpt-4-0125-preview
summary: XML, eller Extensible Markup Language, har funnits sedan slutet av 90-talet.
title: Att arbeta med XML
weight: 40
---

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
