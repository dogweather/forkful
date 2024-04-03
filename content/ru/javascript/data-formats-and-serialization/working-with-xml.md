---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:19.203822-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412\u043E\u0442 \u043A\u0430\u043A \u0440\u0430\u0437\u043E\u0431\
  \u0440\u0430\u0442\u044C XML."
lastmod: '2024-03-13T22:44:45.803344-06:00'
model: gpt-4-0125-preview
summary: "\u0412\u043E\u0442 \u043A\u0430\u043A \u0440\u0430\u0437\u043E\u0431\u0440\
  \u0430\u0442\u044C XML."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 XML"
weight: 40
---

## Как это сделать:
Вот как разобрать XML:

```javascript
let parser = new DOMParser();
let xmlString = `<note>
                    <to>Пользователь</to>
                    <from>Автор</from>
                    <heading>Напоминание</heading>
                    <body>Не забудьте про меня на этом выходных!</body>
                 </note>`;

let xmlDoc = parser.parseFromString(xmlString, "application/xml");
console.log(xmlDoc.getElementsByTagName('to')[0].childNodes[0].nodeValue);
// Вывод: Пользователь
```

И как создать XML:

```javascript
let xmlDocument = document.implementation.createDocument('', '', null);
let noteElement = xmlDocument.createElement('note');
noteElement.appendChild(xmlDocument.createElement('to')).textContent = 'Пользователь';
xmlDocument.appendChild(noteElement);
let serializer = new XMLSerializer();
let xmlString = serializer.serializeToString(xmlDocument);
console.log(xmlString);
// Вывод: <note><to>Пользователь</to></note>
```

## Глубокое погружение
XML - это сокращение от Extensible Markup Language (расширяемый язык разметки), формат данных, появившийся в конце 90-х. Он определяет набор правил для кодирования документов, которые могут читать как люди, так и машины. Исторически XML получил распространение благодаря своей гибкости и структурированной иерархии, что сделало его выбором для веб-сервисов, таких как SOAP, и множества файлов конфигурации.

Альтернативы XML включают JSON (JavaScript Object Notation), который стал популярным за его легкость в использовании с JavaScript и меньший вес. YAML - еще одна альтернатива, ценится за дружественность к человеку и частое использование для конфигурации.

XML реализуется в JavaScript с использованием интерфейсов DOMParser и XMLSerializer. XML DOM (Document Object Model) позволяет навигировать и редактировать документы XML так же, как вы бы это делали с HTML. Несмотря на рост популярности JSON, понимание XML ключевое, так как множество устаревших систем и специфических отраслей все еще полагаются на него для обмена данными.

## Смотрите также
- MDN Web Docs (Разбор XML): https://developer.mozilla.org/en-US/docs/Web/API/DOMParser
- W3Schools (Учебник XML DOM): https://www.w3schools.com/xml/dom_intro.asp
- "Что такое XML?": https://www.w3.org/XML/
