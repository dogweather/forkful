---
title:                "Работа с TOML"
aliases: - /ru/javascript/working-with-toml.md
date:                  2024-01-29T00:04:32.741947-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/javascript/working-with-toml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
TOML, сокращение от Tom's Obvious, Minimal Language (очевидный минималистичный язык Тома), определяет, как структурировать конфигурационные файлы. Программисты работают с TOML, потому что он легко читается, пишется и хорошо соотносится с хеш-таблицей, делая его предпочтительным выбором для конфигураций.

## Как:
Чтобы работать с TOML в JavaScript, вам понадобится парсер, такой как `@iarna/toml`. Сначала установите его: `npm install @iarna/toml`. Затем, преобразуйте строку TOML в объект JavaScript или объект JavaScript в строку формата TOML.

```javascript
const toml = require('@iarna/toml');

// Преобразование строки TOML в объект JS
const tomlStr = `
title = "Пример TOML"

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
`;

const parsedData = toml.parse(tomlStr);
console.log(parsedData);

// Конвертация объекта JS в строку TOML
const jsObject = {
  title: "Пример TOML",
  database: {
    server: "192.168.1.1",
    ports: [8001, 8001, 8002]
  }
};

const tomlString = toml.stringify(jsObject);
console.log(tomlString);
```

## Подробнее
TOML был впервые выпущен в 2013 году Томом Престон-Вернером, одним из сооснователей GitHub. Он был создан для замены других форматов, таких как INI, благодаря более стандартизированному и легкому для разбора формату. JSON и YAML являются альтернативами, но могут быть слишком сложными или слишком гибкими. Преимущество TOML заключается в статической конфигурации, где предпочитается простой и понятный формат. Его дизайн позволяет просто отображать в хеш-таблицу, где ключи и значения соответствуют именам свойств и их значениям. Для более широкого принятия вам может потребоваться интегрировать инструменты, которые могут конвертировать между TOML и другими форматами из-за различной поддержки в экосистемах.

## Смотрите также
- Официальный репозиторий TOML на GitHub: https://github.com/toml-lang/toml
- Сравнение TOML vs. YAML vs. JSON: https://gist.github.com/oconnor663/9aeb4ed56394cb013a20
- npm пакет `@iarna/toml`: https://www.npmjs.com/package/@iarna/toml
