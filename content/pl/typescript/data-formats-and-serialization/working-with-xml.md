---
date: 2024-01-26 04:36:46.287204-07:00
description: "Jak to zrobi\u0107: XML, czyli Rozszerzalny J\u0119zyk Znacznik\xF3\
  w, istnieje od ko\u0144ca lat 90. Jego samoopisowa natura i format czytelny dla\
  \ cz\u0142owieka szybko zyska\u0142y\u2026"
lastmod: '2024-04-05T21:53:36.604569-06:00'
model: gpt-4-0125-preview
summary: "XML, czyli Rozszerzalny J\u0119zyk Znacznik\xF3w, istnieje od ko\u0144ca\
  \ lat 90."
title: Praca z XML
weight: 40
---

## Jak to zrobić:
```TypeScript
import { parseString } from 'xml2js';

// Przykładowy XML
const xml = `<note>
                <to>User</to>
                <from>Author</from>
                <heading>Reminder</heading>
                <body>Nie zapomnij o spotkaniu!</body>
             </note>`;

// Parsowanie XML na JSON
parseString(xml, (err, result) => {
    if(err) throw err;
    console.log(result);
});

// Zakładając, że parsowanie było pomyślne, wynik może wyglądać tak:
// { note:
//    { to: ['User'],
//      from: ['Author'],
//      heading: ['Reminder'],
//      body: ['Nie zapomnij o spotkaniu!'] } 
}
```

## Pogłębiona analiza
XML, czyli Rozszerzalny Język Znaczników, istnieje od końca lat 90. Jego samoopisowa natura i format czytelny dla człowieka szybko zyskały na popularności w różnych zastosowaniach, takich jak kanały RSS, zarządzanie konfiguracją, a nawet formaty dokumentów biurowych, takie jak Microsoft Office Open XML. Ale jest rozwlekły w porównaniu do JSON i obecnie następuje zmiana. JSON zyskał rozgłos wśród interfejsów API opartych na sieci web ze względu na jego mniejszą wagę i natywną kompatybilność z JavaScript.

Niemniej jednak, XML nie umarł. Jest używany w dużych systemach przedsiębiorstw i dla standardów dokumentów, które nie przeszły na JSON. Narzędzia takie jak `xml2js` dla TypeScript czy `lxml` w Pythonie dowodzą, że istnieje ciągła potrzeba manipulacji XML w programowaniu.

TypeScript nie ma wbudowanego wsparcia dla XML, tak jak ma dla JSON. Zamiast tego pracujesz z bibliotekami. `xml2js` jest przykładem. Przekształca XML na JSON, co ułatwia pracę z danymi dla specjalistów od JavaScript.

## Zobacz również
- [Dokumentacja MDN na temat XML](https://developer.mozilla.org/en-US/docs/Web/XML/XML_introduction)
- [Pakiet xml2js npm](https://www.npmjs.com/package/xml2js)
- [Samouczek XML z W3Schools](https://www.w3schools.com/xml/)
