---
title:                "Arbeiten mit XML"
aliases:
- /de/typescript/working-with-xml.md
date:                  2024-01-26T04:36:08.737058-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/working-with-xml.md"
---

{{< edit_this_page >}}

## Was & Warum?
Mit XML zu arbeiten bedeutet, XML-Daten mithilfe der Programmierung zu parsen, zu manipulieren und zu schreiben. Programmierer handhaben XML, um Daten zwischen verschiedenen Systemen auszutauschen, für Konfigurationsdateien, oder wenn sie mit Standards wie SOAP arbeiten, die auf XML basieren.

## Wie geht das:
```TypeScript
import { parseString } from 'xml2js';

// Beispiel XML
const xml = `<note>
                <to>User</to>
                <from>Author</from>
                <heading>Erinnerung</heading>
                <body>Vergiss das Meeting nicht!</body>
             </note>`;

// XML in JSON parsen
parseString(xml, (err, result) => {
    if(err) throw err;
    console.log(result);
});

// Angenommen, das Parsen war erfolgreich, könnte die Ausgabe so aussehen:
// { note:
//    { to: ['User'],
//      from: ['Author'],
//      heading: ['Erinnerung'],
//      body: ['Vergiss das Meeting nicht!'] } 
}
```

## Tiefere Einblicke
XML oder Extensible Markup Language gibt es seit den späten 90ern. Seine selbstbeschreibende Natur und das für Menschen lesbare Format machten es frühzeitig für verschiedene Anwendungen wie RSS-Feeds, Konfigurationsmanagement und sogar Bürodokumentformate wie Microsoft Office Open XML beliebt. Aber es ist umständlicher im Vergleich zu JSON, und die Zeiten ändern sich. JSON hat die Aufmerksamkeit für webbasierte APIs auf sich gezogen aufgrund seines geringeren Gewichts und der nativen JavaScript-Kompatibilität.

Dennoch ist XML nicht tot. Es wird in groß angelegten Unternehmenssystemen und für Dokumentstandards verwendet, die nicht auf JSON umgestellt haben. Tools wie `xml2js` für TypeScript oder `lxml` in Python beweisen, dass es einen anhaltenden Bedarf für die Manipulation von XML in der Programmierung gibt.

TypeScript hat keine eingebaute Unterstützung für XML wie es sie für JSON gibt. Stattdessen arbeitet man mit Bibliotheken. `xml2js` ist ein Beispiel. Es transformiert XML in JSON und macht die Daten leichter für JavaScript-Kenner zu handhaben.

## Siehe auch
- [MDN Web Docs zu XML](https://developer.mozilla.org/en-US/docs/Web/XML/XML_introduction)
- [xml2js npm-Paket](https://www.npmjs.com/package/xml2js)
- [W3Schools XML-Tutorial](https://www.w3schools.com/xml/)
