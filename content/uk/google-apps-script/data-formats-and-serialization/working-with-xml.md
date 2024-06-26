---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:07:03.701870-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : Google Apps Script \u043D\u0430\u0434\u0430\u0454 `XmlService` \u0434\u043B\u044F\
  \ \u0440\u043E\u0431\u043E\u0442\u0438 \u0437 \u0434\u0430\u043D\u0438\u043C\u0438\
  \ XML. \u041D\u0438\u0436\u0447\u0435 \u043C\u0438 \u0434\u0435\u043C\u043E\u043D\
  \u0441\u0442\u0440\u0443\u0454\u043C\u043E, \u044F\u043A \u0430\u043D\u0430\u043B\
  \u0456\u0437\u0443\u0432\u0430\u0442\u0438 \u0440\u044F\u0434\u043E\u043A XML, \u0437\
  \u043C\u0456\u043D\u044E\u0432\u0430\u0442\u0438 \u0439\u043E\u0433\u043E \u0432\
  \u043C\u0456\u0441\u0442 \u0442\u0430\u2026"
lastmod: '2024-03-13T22:44:48.554125-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script \u043D\u0430\u0434\u0430\u0454 `XmlService` \u0434\u043B\
  \u044F \u0440\u043E\u0431\u043E\u0442\u0438 \u0437 \u0434\u0430\u043D\u0438\u043C\
  \u0438 XML."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 XML"
weight: 40
---

## Як це зробити:
Google Apps Script надає `XmlService` для роботи з даними XML. Нижче ми демонструємо, як аналізувати рядок XML, змінювати його вміст та генерувати новий рядок XML.

Аналіз рядка XML:

```javascript
function parseXML() {
  var xmlString = '<root><child name="first">Hello</child><child name="second">World</child></root>';
  var document = XmlService.parse(xmlString);
  var root = document.getRootElement();
  var children = root.getChildren('child');
  Logger.log(children[0].getText()); // Логує: Hello
}
```

Щоб змінити XML, ви можете захотіти додати новий дочірній елемент:

```javascript
function addNewChild() {
  var xmlString = '<root><child name="first">Hello</child></root>';
  var document = XmlService.parse(xmlString);
  var root = document.getRootElement();
  
  var newChild = XmlService.createElement('child').setText('World');
  root.addContent(newChild);
  
  var xml = XmlService.getPrettyFormat().format(document);
  Logger.log(xml);
  // Логує новий рядок XML з доданим дочірнім елементом
}
```

Генерація рядка XML з нуля:

```javascript
function createXML() {
  var root = XmlService.createElement('root');
  var child = XmlService.createElement('child').setText('Hello World');
  root.addContent(child);
  
  var xml = XmlService.getPrettyFormat().format(XmlService.createDocument(root));
  Logger.log(xml);
  // Виводить: <root><child>Hello World</child></root>
}
```

## Поглиблений розгляд
Історично XML (Розширювана Мова Розмітки) була стандартом де-факто для обміну даними до появи JSON як легкої альтернативи. Вербозна синтаксис і строга модель аналізу XML забезпечували надійний, хоча й об'ємний, формат даних. У Google Apps Script API `XmlService` включає створення, аналіз та маніпуляцію даними XML, визнаючи їхнє продовжене значення у різноманітних старих та корпоративних системах, SOAP веб-сервісах та файлах конфігурацій для додатків.

Незважаючи на перевагу JSON у сучасній веб-розробці за її простоту і легкість використання з JavaScript, XML залишається актуальним у сферах, де важливі валідація документів і структуровані ієрархії. Однак для нових проектів, особливо тих, що схильні до веб-API, JSON часто є більш практичним вибором через його легку вагу та безшовну інтеграцію з JavaScript.

Розуміння XML та його обробки в Google Apps Script є важливим для розробників, які працюють у середовищах, де необхідна інтеграція зі старими системами або специфічними корпоративними API. Однак, починаючи нові проекти або коли ключовим є гнучкість, рекомендується оцінити потребу в XML порівняно з альтернативами, такими як JSON.
