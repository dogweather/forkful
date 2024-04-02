---
date: 2024-01-26 04:33:05.773230-07:00
description: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 XML \u043E\u0437\u043D\u0430\
  \u0447\u0430\u0454 \u0430\u043D\u0430\u043B\u0456\u0437, \u043C\u0430\u043D\u0456\
  \u043F\u0443\u043B\u044F\u0446\u0456\u044E \u0442\u0430 \u0441\u0442\u0432\u043E\
  \u0440\u0435\u043D\u043D\u044F \u0432\u043C\u0456\u0441\u0442\u0443 XML \u0437\u0430\
  \ \u0434\u043E\u043F\u043E\u043C\u043E\u0433\u043E\u044E \u043A\u043E\u0434\u0443\
  . \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0440\u043E\
  \u0431\u043B\u044F\u0442\u044C \u0446\u0435 \u0447\u0435\u0440\u0435\u0437 \u0442\
  \u0435, \u0449\u043E XML \u0448\u0438\u0440\u043E\u043A\u043E \u0432\u0438\u043A\
  \u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0454\u0442\u044C\u0441\u044F \u0434\
  \u043B\u044F\u2026"
lastmod: '2024-03-13T22:44:50.035333-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 XML \u043E\u0437\u043D\u0430\
  \u0447\u0430\u0454 \u0430\u043D\u0430\u043B\u0456\u0437, \u043C\u0430\u043D\u0456\
  \u043F\u0443\u043B\u044F\u0446\u0456\u044E \u0442\u0430 \u0441\u0442\u0432\u043E\
  \u0440\u0435\u043D\u043D\u044F \u0432\u043C\u0456\u0441\u0442\u0443 XML \u0437\u0430\
  \ \u0434\u043E\u043F\u043E\u043C\u043E\u0433\u043E\u044E \u043A\u043E\u0434\u0443\
  . \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0440\u043E\
  \u0431\u043B\u044F\u0442\u044C \u0446\u0435 \u0447\u0435\u0440\u0435\u0437 \u0442\
  \u0435, \u0449\u043E XML \u0448\u0438\u0440\u043E\u043A\u043E \u0432\u0438\u043A\
  \u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0454\u0442\u044C\u0441\u044F \u0434\
  \u043B\u044F\u2026"
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 XML"
weight: 40
---

## Що і чому?
Робота з XML означає аналіз, маніпуляцію та створення вмісту XML за допомогою коду. Програмісти роблять це через те, що XML широко використовується для файлів конфігурації, обміну даними та веб-сервісів завдяки його зручності для сприйняття людьми та парсингу машинами.

## Як це зробити:

Ось як парсити XML:

```javascript
let parser = new DOMParser();
let xmlString = `<note>
                    <to>User</to>
                    <from>Author</from>
                    <heading>Reminder</heading>
                    <body>Не забудь про мене цими вихідними!</body>
                 </note>`;

let xmlDoc = parser.parseFromString(xmlString, "application/xml");
console.log(xmlDoc.getElementsByTagName('to')[0].childNodes[0].nodeValue);
// Вивід: User
```

І для того, щоб створити XML:

```javascript
let xmlDocument = document.implementation.createDocument('', '', null);
let noteElement = xmlDocument.createElement('note');
noteElement.appendChild(xmlDocument.createElement('to')).textContent = 'User';
xmlDocument.appendChild(noteElement);
let serializer = new XMLSerializer();
let xmlString = serializer.serializeToString(xmlDocument);
console.log(xmlString);
// Вивід: <note><to>User</to></note>
```

## Поглиблено

XML — це скорочення від eXtensible Markup Language, формат даних, який з'явився наприкінці 90-х. Він визначає набір правил для кодування документів, які могуть читати як люди, так і машини. Історично XML набув популярності через свою гнучкість та структуровану ієрархію, що зробило його вибором для веб-сервісів, таких як SOAP, та численних файлів конфігурації.

Альтернативами XML є JSON (JavaScript Object Notation), який став популярним завдяки своїй простоті використання з JavaScript і меншій вазі. YAML — це ще одна альтернатива, цінується за її привітність до людини та поширене використання для конфігурації.

XML реалізований в JavaScript за допомогою інтерфейсів DOMParser та XMLSerializer. XML DOM (модель об'єкта документа) дозволяє навігувати і редагувати документи XML так, як ви б робили це з HTML. Незважаючи на зростання популярності JSON, розуміння XML є ключовим, оскільки багато застарілих систем та конкретні галузі досі покладаються на нього для обміну даними.

## Див. також

- MDN Web Docs (Парсинг XML): https://developer.mozilla.org/en-US/docs/Web/API/DOMParser
- W3Schools (Посібник з XML DOM): https://www.w3schools.com/xml/dom_intro.asp
- "Що таке XML?": https://www.w3.org/XML/
