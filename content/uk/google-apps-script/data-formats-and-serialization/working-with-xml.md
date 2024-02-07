---
title:                "Робота з XML"
date:                  2024-02-01T22:07:03.701870-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з XML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/google-apps-script/working-with-xml.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та чому?

Робота з XML у Google Apps Script дозволяє програмістам аналізувати, маніпулювати та генерувати дані XML, що є невід'ємним для веб-сервісів та конфігурацій. Програмісти обирають цей підхід для інтеграції зі старими системами, виконання веб-скрапінгу або комунікації з численними API, які все ще використовують XML замість JSON для обміну даними.

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
