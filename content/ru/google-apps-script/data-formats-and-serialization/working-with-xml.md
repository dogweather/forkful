---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:53.338987-07:00
description: "\u041A\u0430\u043A: Google Apps Script \u043F\u0440\u0435\u0434\u043E\
  \u0441\u0442\u0430\u0432\u043B\u044F\u0435\u0442 `XmlService` \u0434\u043B\u044F\
  \ \u0440\u0430\u0431\u043E\u0442\u044B \u0441 \u0434\u0430\u043D\u043D\u044B\u043C\
  \u0438 XML. \u041D\u0438\u0436\u0435 \u043C\u044B \u0434\u0435\u043C\u043E\u043D\
  \u0441\u0442\u0440\u0438\u0440\u0443\u0435\u043C, \u043A\u0430\u043A \u0430\u043D\
  \u0430\u043B\u0438\u0437\u0438\u0440\u043E\u0432\u0430\u0442\u044C \u0441\u0442\u0440\
  \u043E\u043A\u0443 XML, \u0438\u0437\u043C\u0435\u043D\u044F\u0442\u044C \u0435\u0435\
  \ \u0441\u043E\u0434\u0435\u0440\u0436\u0438\u043C\u043E\u0435 \u0438\u2026"
lastmod: '2024-03-13T22:44:44.237441-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script \u043F\u0440\u0435\u0434\u043E\u0441\u0442\u0430\u0432\
  \u043B\u044F\u0435\u0442 `XmlService` \u0434\u043B\u044F \u0440\u0430\u0431\u043E\
  \u0442\u044B \u0441 \u0434\u0430\u043D\u043D\u044B\u043C\u0438 XML."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 XML"
weight: 40
---

## Как:
Google Apps Script предоставляет `XmlService` для работы с данными XML. Ниже мы демонстрируем, как анализировать строку XML, изменять ее содержимое и генерировать новую строку XML.

Анализ строки XML:

```javascript
function parseXML() {
  var xmlString = '<root><child name="first">Привет</child><child name="second">Мир</child></root>';
  var document = XmlService.parse(xmlString);
  var root = document.getRootElement();
  var children = root.getChildren('child');
  Logger.log(children[0].getText()); // Выводит в журнал: Привет
}
```

Чтобы изменить XML, вы можете добавить новый дочерний элемент:

```javascript
function addNewChild() {
  var xmlString = '<root><child name="first">Привет</child></root>';
  var document = XmlService.parse(xmlString);
  var root = document.getRootElement();
  
  var newChild = XmlService.createElement('child').setText('Мир');
  root.addContent(newChild);
  
  var xml = XmlService.getPrettyFormat().format(document);
  Logger.log(xml);
  // Выводит в журнал новую строку XML с добавленным дочерним элементом
}
```

Генерация строки XML с нуля:

```javascript
function createXML() {
  var root = XmlService.createElement('root');
  var child = XmlService.createElement('child').setText('Привет Мир');
  root.addContent(child);
  
  var xml = XmlService.getPrettyFormat().format(XmlService.createDocument(root));
  Logger.log(xml);
  // Выводит: <root><child>Привет Мир</child></root>
}
```

## Погружение
Исторически XML (Расширяемый язык разметки) был де-факто стандартом для обмена данными до того, как JSON появился как легковесная альтернатива. Многословный синтаксис XML и строгая модель разбора предоставляли надежный, хотя и громоздкий, формат данных. В Google Apps Script API `XmlService` инкапсулирует создание, анализ и изменение данных XML, признавая их продолжающуюся важность в различных устаревших и корпоративных системах, веб-сервисах SOAP и файлах конфигурации для приложений.

Несмотря на распространенность JSON в современной веб-разработке из-за его простоты и удобства использования с JavaScript, XML остается актуальным в областях, где важны проверка документов и структурированные иерархии. Однако для новых проектов, особенно тех, которые ориентированы на веб-API, JSON часто является более практичным выбором из-за его легковесности и бесшовной интеграции с JavaScript.

Понимание XML и его обработки в Google Apps Script крайне важно для разработчиков, работающих в средах, где необходима интеграция со старыми системами или специфическими корпоративными API. Однако, начиная новые проекты или когда ключевым является гибкость, рекомендуется оценить необходимость использования XML перед альтернативами, такими как JSON.
