---
title:                "Работа с XML"
aliases:
- ru/google-apps-script/working-with-xml.md
date:                  2024-02-01T22:06:53.338987-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с XML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/google-apps-script/working-with-xml.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Работа с XML в Google Apps Script позволяет программистам анализировать, изменять и создавать данные XML, что необходимо для веб-сервисов и конфигураций. Программисты используют этот подход для интеграции с устаревшими системами, выполнения веб-скрапинга или взаимодействия с множеством API, которые до сих пор предпочитают XML вместо JSON для обмена данными.

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
