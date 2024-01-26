---
title:                "Робота з XML"
date:                  2024-01-26T04:33:05.773230-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з XML"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/working-with-xml.md"
---

{{< edit_this_page >}}

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