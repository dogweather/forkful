---
title:                "XML:n käsittely"
aliases:
- fi/typescript/working-with-xml.md
date:                  2024-01-26T04:36:13.552322-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML:n käsittely"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/working-with-xml.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Työskentely XML:n parissa tarkoittaa XML-tiedon jäsentämistä, manipulointia ja kirjoittamista ohjelmoinnin avulla. Ohjelmoijat käsittelevät XML:ää vaihtaakseen tietoja eri järjestelmien välillä, käyttöön konfiguraatiotiedostoissa tai kun työskentelevät standardien, kuten SOAP, parissa, jotka nojaavat XML:ään.

## Kuinka:
```TypeScript
import { parseString } from 'xml2js';

// Esimerkki XML
const xml = `<note>
                <to>User</to>
                <from>Author</from>
                <heading>Reminder</heading>
                <body>Älä unohda kokousta!</body>
             </note>`;

// Jäsennä XML JSON:ksi
parseString(xml, (err, result) => {
    if(err) throw err;
    console.log(result);
});

// Olettaen, että jäsentäminen onnistui, tuloste näyttäisi tältä:
// { note:
//    { to: ['User'],
//      from: ['Author'],
//      heading: ['Reminder'],
//      body: ['Älä unohda kokousta!'] }
}
```

## Syväsukellus
XML eli Laajennettava Merkkauskieli on ollut olemassa 90-luvun lopulta lähtien. Sen itsensä kuvaava luonne ja ihmisen luettava muoto tekivät siitä nopeasti suositun erilaisille sovelluksille, kuten RSS-syötteille, konfiguraationhallinnalle ja jopa toimistoasiakirjamuotoille, kuten Microsoft Office Open XML:lle. Se on kuitenkin verbosinen verrattuna JSON:iin, ja ajan saatossa suosio on kääntynyt. JSON on saanut huomion web-pohjaisissa API:issa sen keveyden ja natiivin JavaScript-yhteensopivuuden vuoksi.

Silti, XML ei ole kuollut. Sitä käytetään suurissa yritysjärjestelmissä ja dokumenttistandardeissa, jotka eivät ole siirtyneet JSON:iin. Työkalut, kuten `xml2js` TypeScriptille tai `lxml` Pythonille, todistavat, että XML:n käsittelyn tarve ohjelmoinnissa jatkuu.

TypeScript ei tue XML:ää sisäänrakennetusti kuten se tukee JSON:ia. Sen sijaan työskentelet kirjastoilla. `xml2js` on esimerkki. Se muuntaa XML:n JSON:ksi, tehdessään tiedosta helpommin käsiteltävää JavaScript-osaajille.

## Katso Myös
- [MDN Web Docs XML:stä](https://developer.mozilla.org/en-US/docs/Web/XML/XML_introduction)
- [xml2js npm -paketti](https://www.npmjs.com/package/xml2js)
- [W3Schoolsin XML-oppitunti](https://www.w3schools.com/xml/)
