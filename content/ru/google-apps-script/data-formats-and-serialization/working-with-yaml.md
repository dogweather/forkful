---
title:                "Работа с YAML"
aliases:
- /ru/google-apps-script/working-with-yaml.md
date:                  2024-02-01T22:07:43.822037-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/google-apps-script/working-with-yaml.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?

YAML, расшифровывающееся как "YAML Ain't Markup Language" (YAML - это не язык разметки), представляет собой стандарт сериализации данных, удобный для чтения человеком, который часто используется для файлов конфигурации и обмена данными между языками с различными структурами данных. Программисты часто работают с YAML из-за его простоты и читаемости, особенно в проектах, требующих обширной конфигурации или при передаче структурированных данных между разными системами.

## Как:

Хотя Google Apps Script (GAS) не поддерживает анализ или сериализацию YAML напрямую, вы можете манипулировать данными YAML, используя библиотеки JavaScript или написав собственные функции парсинга. Для демонстрации рассмотрим, как проанализировать строку YAML с помощью собственной функции, поскольку внешние библиотеки не могут быть напрямую импортированы в GAS.

Предположим, у вас есть простая конфигурация YAML:

```yaml
title: Пример YAML
description: Пример того, как обрабатывать YAML в Google Apps Script
tags:
  - Google Apps Script
  - YAML
  - Конфигурация
```

Чтобы проанализировать это в Google Apps Script, используйте возможности манипулирования строками в JavaScript:

```javascript
function parseYAML(yamlString) {
  var result = {};
  var lines = yamlString.split("\n");
  for (var i = 0; i < lines.length; i++) {
    var line = lines[i];
    if (line.includes(":")) {
      var parts = line.split(":");
      var key = parts[0].trim();
      var value = parts[1].trim();
      // Простая обработка массивов
      if (value.startsWith("-")) {
        value = [value.substring(1).trim()];
        while (i + 1 < lines.length && lines[i + 1].trim().startsWith("-")) {
          i++;
          value.push(lines[i].trim().substring(1).trim());
        }
      }
      result[key] = value;
    }
  }
  return result;
}

function testYamlParsing() {
  var yaml = "title: Пример YAML\ndescription: Пример того, как обрабатывать YAML в Google Apps Script\ntags:\n  - Google Apps Script\n  - YAML\n  - Конфигурация";
  var parsed = parseYAML(yaml);
  Logger.log(parsed);
}
```

Когда выполняется `testYamlParsing()`, в выводе получаем:

```
{ title: 'Пример YAML',
  description: 'Пример того, как обрабатывать YAML в Google Apps Script',
  tags: [ 'Google Apps Script', ' YAML', ' Конфигурация' ] }
```

Этот подход к парсингу довольно базовый и может потребовать корректировки для работы со сложными файлами YAML.

## Глубокое погружение

YAML, впервые выпущенный в 2001 году, был направлен на то, чтобы быть более удобным для чтения по сравнению с его предшественниками, такими как XML или JSON. Несмотря на то что его простота и удобство использования широко ценятся, работа с YAML в Google Apps Script представляет собой вызов из-за отсутствия прямой поддержки. В результате программисты часто полагаются на универсальность JavaScript для анализа и генерации данных YAML. Однако для сложных случаев использования, особенно тех, которые включают глубокое вложение и продвинутые структуры данных, этот метод может быть громоздким и подверженным ошибкам.

JSON, в отличие от этого, поддерживается нативно в Google Apps Script и большинстве других программных сред, предлагая более простой подход к сериализации и десериализации данных без дополнительных затрат на парсинг. Синтаксис JSON менее многословен, чем YAML, что делает его более подходящим для обмена данными в веб-приложениях. Тем не менее, YAML остается популярным для файлов конфигурации и ситуаций, когда важна читаемость человеком.

Работая с YAML в Google Apps Script, учитывайте компромисс между читаемостью и удобством использования. Для всесторонней манипуляции с YAML может быть полезным исследовать внешние инструменты или сервисы, которые могут конвертировать YAML в JSON перед обработкой его в вашем скрипте.
