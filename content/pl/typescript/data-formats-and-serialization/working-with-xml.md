---
title:                "Praca z XML"
date:                  2024-01-26T04:36:46.287204-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/working-with-xml.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Praca z XML oznacza parsowanie, manipulację i zapisywanie danych XML za pomocą programowania. Programiści obsługują XML, aby wymieniać dane między różnymi systemami, dla plików konfiguracyjnych, lub gdy pracują ze standardami takimi jak SOAP, które opierają się na XML.

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
