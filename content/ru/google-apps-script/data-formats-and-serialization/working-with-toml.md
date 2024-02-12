---
title:                "Работа с TOML"
aliases:
- /ru/google-apps-script/working-with-toml/
date:                  2024-02-01T22:05:55.899424-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с TOML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/google-apps-script/working-with-toml.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?

TOML, что расшифровывается как Tom's Obvious, Minimal Language (Явный, Минимальный Язык Тома), является форматом файла конфигурации, который легко читается благодаря своей ясной семантике. Программисты часто используют его для файлов конфигурации в приложениях, потому что он прост и понятен человеку, что делает управление настройками и конфигурациями приложения безупречным в различных средах.

## Как это сделать:

Поскольку Google Apps Script по существу является JavaScript с доступом к набору приложений Google, работа с TOML непосредственно в Google Apps Script требует немного находчивости. Google Apps Script изначально не поддерживает анализ TOML, но вы можете использовать библиотеки JavaScript или написать простой парсер для базовых нужд.

Давайте разберем простую строку конфигурации TOML в качестве примера:

```javascript
// Строка TOML
var tomlString = `
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
`;

// Простая функция парсера TOML в JSON
function parseTOML(tomlStr) {
  var result = {};
  var currentSection = result;
  tomlStr.split(/\r?\n/).forEach(line => {
    line = line.trim();
    if (line.startsWith('[')) { // Новая секция
      var sectionName = line.replace(/\[|\]/g, '');
      result[sectionName] = {};
      currentSection = result[sectionName];
    } else if (line) {
      var keyValue = line.split('=').map(part => part.trim());
      var key = keyValue[0];
      var value = eval(keyValue[1]); // Используем eval для упрощения; будьте осторожны в производственном коде
      currentSection[key] = value;
    }
  });
  return result;
}

// Тестирование парсера
var configObject = parseTOML(tomlString);
console.log(configObject);

```

Пример вывода из `console.log` будет напоминать объект JSON, что упрощает доступ к свойствам конфигурации в Google Apps Script:

```json
{
  "database": {
    "server": "192.168.1.1",
    "ports": [8001, 8001, 8002],
    "connection_max": 5000,
    "enabled": true
  }
}
```

## Глубокое погружение

TOML был создан Томом Престон-Вернером, одним из основателей GitHub, чтобы быть более удобным для человека, чем JSON, для файлов конфигурации, сохраняя при этом возможность однозначного анализа. Его целью является максимальное упрощение, цель, которая хорошо согласуется с этикой многих разработческих проектов, стремящихся к простоте и читабельности своих кодовых баз.

В контексте Google Apps Script использование TOML может ввести некоторые издержки, учитывая отсутствие прямой поддержки и необходимость его анализировать вручную или с использованием сторонних библиотек. Для меньших проектов или тех, которые не глубоко интегрированы в экосистему Google, альтернативы, такие как JSON или даже простые структуры пар ключ-значение в свойствах скрипта, могут оказаться достаточными и более простыми в реализации. Однако для приложений, которые отдают предпочтение удобным для человека файлам конфигурации и уже привержены TOML, интеграция анализа TOML через пользовательские скрипты добавляет полезный слой гибкости и удобства в обслуживании, не отходя от предпочтительных парадигм конфигурации.
