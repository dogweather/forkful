---
title:                "Работа с JSON"
aliases:
- ru/google-apps-script/working-with-json.md
date:                  2024-02-01T22:05:50.239324-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/google-apps-script/working-with-json.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

JSON, или JavaScript Object Notation, это легковесный формат для хранения и передачи данных, идеален для связи сервер-клиент и файлов конфигурации. Программисты используют его в Google Apps Script для бесперебойного обмена данными между сервисами Google (такими как Таблицы, Документы, Диск) и внешними источниками, благодаря его читабельной структуре и легкой интеграции в среде на основе JavaScript.

## Как:

В Google Apps Script работа с JSON происходит просто, во многом благодаря встроенной поддержке, которую JavaScript предоставляет для разбора и преобразования JSON в строку. Вот некоторые общие операции:

**1. Разбор JSON**: Предположим, мы получаем строку JSON от веб-сервиса; разбор ее в объект JavaScript необходим для манипуляции с данными.

```javascript
var jsonString = '{"name": "Sample Project", "version": "1.0.0"}';
var obj = JSON.parse(jsonString);
Logger.log(obj.name); // Вывод: Sample Project
```

**2. Преобразование объектов JavaScript в строку JSON**: Наоборот, преобразование объекта JavaScript в строку JSON полезно, когда нам нужно отправить данные из Apps Script во внешний сервис.

```javascript
var projectData = {
  name: "Sample Project",
  version: "1.0.0"
};
var jsonString = JSON.stringify(projectData);
Logger.log(jsonString); // Вывод: '{"name":"Sample Project","version":"1.0.0"}'
```

**3. Работа со сложными данными**:
Для более сложных структур данных, таких как массивы объектов, процесс остается тем же, демонстрируя гибкость JSON для представления данных.

```javascript
var projects = [
  {name: "Project 1", version: "1.0"},
  {name: "Project 2", version: "2.0"}
];
var jsonString = JSON.stringify(projects);
Logger.log(jsonString); // Вывод: '[{"name":"Project 1","version":"1.0"},{"name":"Project 2","version":"2.0"}]'
```

## Погружение

Всеобщее присутствие JSON в современных веб-приложениях не может быть переоценено, укоренившееся в его простоте и насколько бесшовно он интегрируется с JavaScript, языком веба. Его дизайн, вдохновленный литералами объектов JavaScript, хотя и строже, облегчает его быстрое принятие. В начале 2000-х годов JSON стал популярным как альтернатива XML для веб-приложений, работающих на AJAX, предлагая более легковесный и менее многословный формат обмена данными. Учитывая глубокую интеграцию Google Apps Script с различными API Google и внешними сервисами, JSON служит ключевым форматом для структурирования, передачи и манипуляции данными на этих платформах.

Хотя JSON доминирует в веб-приложениях, существуют альтернативные форматы данных, такие как YAML для файлов конфигурации или Protobuf для более эффективной двоичной сериализации в средах с высокой производительностью. Однако баланс читабельности, легкости использования и широкой поддержки в разных языках программирования и инструментах закрепляет позицию JSON как стандартного выбора для многих разработчиков, вовлеченных в работу с Google Apps Script и за его пределами.
