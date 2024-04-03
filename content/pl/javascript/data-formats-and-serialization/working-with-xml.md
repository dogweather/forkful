---
date: 2024-01-26 04:32:40.759514-07:00
description: "Praca z XML oznacza analizowanie, manipulowanie oraz tworzenie zawarto\u015B\
  ci XML przy u\u017Cyciu kodu. Programi\u015Bci robi\u0105 to, poniewa\u017C XML\
  \ jest powszechnie\u2026"
lastmod: '2024-03-13T22:44:35.820655-06:00'
model: gpt-4-0125-preview
summary: "Praca z XML oznacza analizowanie, manipulowanie oraz tworzenie zawarto\u015B\
  ci XML przy u\u017Cyciu kodu."
title: Praca z XML
weight: 40
---

## Co i dlaczego?
Praca z XML oznacza analizowanie, manipulowanie oraz tworzenie zawartości XML przy użyciu kodu. Programiści robią to, ponieważ XML jest powszechnie używany do plików konfiguracyjnych, wymiany danych oraz usług internetowych ze względu na jego czytelną dla ludzi i maszyn przetwarzalność.

## Jak to zrobić:

Oto jak przeprowadzić analizę XML:

```javascript
let parser = new DOMParser();
let xmlString = `<note>
                    <to>Użytkownik</to>
                    <from>Autor</from>
                    <heading>Przypomnienie</heading>
                    <body>Nie zapomnij o mnie tego weekendu!</body>
                 </note>`;

let xmlDoc = parser.parseFromString(xmlString, "application/xml");
console.log(xmlDoc.getElementsByTagName('to')[0].childNodes[0].nodeValue);
// Wyjście: Użytkownik
```

I jak wyprodukować XML:

```javascript
let xmlDocument = document.implementation.createDocument('', '', null);
let noteElement = xmlDocument.createElement('note');
noteElement.appendChild(xmlDocument.createElement('to')).textContent = 'Użytkownik';
xmlDocument.appendChild(noteElement);
let serializer = new XMLSerializer();
let xmlString = serializer.serializeToString(xmlDocument);
console.log(xmlString);
// Wyjście: <note><to>Użytkownik</to></note>
```

## Szczegółowa analiza

XML to skrót od eXtensible Markup Language, format danych, który istnieje od końca lat 90. Definiuje zestaw reguł do kodowania dokumentów, które są czytelne zarówno dla ludzi, jak i maszyn. Historycznie, XML zdobył popularność dzięki swojej elastyczności i strukturyzowanej hierarchii, stając się wyborem dla usług internetowych, takich jak SOAP, oraz licznych plików konfiguracyjnych.

Alternatywami dla XML są JSON (JavaScript Object Notation), który stał się popularny ze względu na łatwość użycia z JavaScriptem i mniejszą wagę. YAML jest inną alternatywą, cenioną za przyjazność dla człowieka oraz powszechny wybór dla konfiguracji.

XML jest implementowany w JavaScript za pomocą interfejsów DOMParser i XMLSerializer. XML DOM (Model Obiektowy Dokumentu) pozwala na nawigowanie i edytowanie dokumentów XML tak, jakbyś to robił z HTML. Pomimo wzrostu popularności JSON, zrozumienie XML jest kluczowe, ponieważ wiele starszych systemów i specyficznych branż nadal polega na nim do wymiany danych.

## Zobacz również

- MDN Web Docs (Analiza XML): https://developer.mozilla.org/pl/docs/Web/API/DOMParser
- W3Schools (Samouczek XML DOM): https://www.w3schools.com/xml/dom_intro.asp
- "Co to jest XML?": https://www.w3.org/XML/
