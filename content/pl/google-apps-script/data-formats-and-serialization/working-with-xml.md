---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:07:49.887735-07:00
description: "Praca z XML w Google Apps Script pozwala programistom na parsowanie,\
  \ manipulowanie i generowanie danych XML, kt\xF3re s\u0105 niezb\u0119dne dla us\u0142\
  ug sieciowych i\u2026"
lastmod: '2024-03-13T22:44:34.930086-06:00'
model: gpt-4-0125-preview
summary: "Praca z XML w Google Apps Script pozwala programistom na parsowanie, manipulowanie\
  \ i generowanie danych XML, kt\xF3re s\u0105 niezb\u0119dne dla us\u0142ug sieciowych\
  \ i\u2026"
title: Praca z XML
weight: 40
---

## Co i dlaczego?

Praca z XML w Google Apps Script pozwala programistom na parsowanie, manipulowanie i generowanie danych XML, które są niezbędne dla usług sieciowych i konfiguracji. Programiści przyjmują to podejście, aby zintegrować się ze starszymi systemami, przeprowadzać web scraping lub komunikować się z licznymi API, które nadal polegają na XML ponad JSON do wymiany danych.

## Jak to zrobić:

Google Apps Script zapewnia `XmlService` do pracy z danymi XML. Poniżej pokazujemy, jak sparsować ciąg XML, zmodyfikować jego zawartość i wygenerować nowy ciąg XML.

Parsowanie ciągu XML:

```javascript
function parseXML() {
  var xmlString = '<root><child name="first">Hello</child><child name="second">World</child></root>';
  var document = XmlService.parse(xmlString);
  var root = document.getRootElement();
  var children = root.getChildren('child');
  Logger.log(children[0].getText()); // Zapisuje: Hello
}
```

Aby zmodyfikować XML, możesz chcieć dodać nowy element dziecko:

```javascript
function addNewChild() {
  var xmlString = '<root><child name="first">Hello</child></root>';
  var document = XmlService.parse(xmlString);
  var root = document.getRootElement();
  
  var newChild = XmlService.createElement('child').setText('World');
  root.addContent(newChild);
  
  var xml = XmlService.getPrettyFormat().format(document);
  Logger.log(xml);
  // Zapisuje nowy ciąg XML z dodanym elementem dziecka
}
```

Generowanie ciągu XML od zera:

```javascript
function createXML() {
  var root = XmlService.createElement('root');
  var child = XmlService.createElement('child').setText('Hello World');
  root.addContent(child);
  
  var xml = XmlService.getPrettyFormat().format(XmlService.createDocument(root));
  Logger.log(xml);
  // Wyjście: <root><child>Hello World</child></root>
}
```

## Głębsze zanurzenie

Historycznie, XML (Extensible Markup Language) był faktycznym standardem do wymiany danych zanim JSON pojawił się jako lekka alternatywa. Wyczerpująca syntaktyka XML i ścisły model parsowania zapewniały solidny, choć obszerny, format danych. W Google Apps Script API `XmlService` enkapsuluje tworzenie, parsowanie i manipulację danymi XML, uznając ich ciągłe znaczenie w różnych starszych i przedsiębiorczych systemach, usługach sieciowych SOAP, oraz plikach konfiguracyjnych dla aplikacji.

Mimo dominacji JSON w nowoczesnym rozwoju stron internetowych ze względu na jego prostotę i łatwość użycia z JavaScript, XML pozostaje istotny w obszarach, gdzie ważna jest walidacja dokumentów i strukturyzowane hierarchie. Jednakże, przy rozpoczynaniu nowych projektów, zwłaszcza tych skłaniających się ku webowym API, JSON jest często praktyczniejszym wyborem z powodu swojej lekkiej natury i bezproblemowej integracji z JavaScript.

Zrozumienie XML i jego obsługi w Google Apps Script jest kluczowe dla programistów pracujących w środowiskach, gdzie potrzebna jest integracja ze starszymi systemami lub specyficznymi przedsiębiorczymi API. Jednakże, przy rozpoczynaniu nowych projektów lub gdy elastyczność jest kluczowa, zaleca się ocenę potrzeby używania XML nad alternatywami takimi jak JSON.
