---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:26.376958-07:00
description: "TOML, \u0447\u0442\u043E \u0440\u0430\u0441\u0448\u0438\u0444\u0440\u043E\
  \u0432\u044B\u0432\u0430\u0435\u0442\u0441\u044F \u043A\u0430\u043A Tom's Obvious,\
  \ Minimal Language (\u041F\u0440\u043E\u0441\u0442\u043E\u0439 \u0438 \u041C\u0438\
  \u043D\u0438\u043C\u0430\u043B\u0438\u0441\u0442\u0438\u0447\u043D\u044B\u0439 \u042F\
  \u0437\u044B\u043A \u0422\u043E\u043C\u0430), - \u044D\u0442\u043E \u0444\u043E\u0440\
  \u043C\u0430\u0442 \u0441\u0435\u0440\u0438\u0430\u043B\u0438\u0437\u0430\u0446\u0438\
  \u0438 \u0434\u0430\u043D\u043D\u044B\u0445, \u043F\u043E\u0434\u043E\u0431\u043D\
  \u044B\u0439 JSON \u0438\u043B\u0438\u2026"
lastmod: '2024-03-13T22:44:44.631210-06:00'
model: gpt-4-0125-preview
summary: "TOML, \u0447\u0442\u043E \u0440\u0430\u0441\u0448\u0438\u0444\u0440\u043E\
  \u0432\u044B\u0432\u0430\u0435\u0442\u0441\u044F \u043A\u0430\u043A Tom's Obvious,\
  \ Minimal Language (\u041F\u0440\u043E\u0441\u0442\u043E\u0439 \u0438 \u041C\u0438\
  \u043D\u0438\u043C\u0430\u043B\u0438\u0441\u0442\u0438\u0447\u043D\u044B\u0439 \u042F\
  \u0437\u044B\u043A \u0422\u043E\u043C\u0430), - \u044D\u0442\u043E \u0444\u043E\u0440\
  \u043C\u0430\u0442 \u0441\u0435\u0440\u0438\u0430\u043B\u0438\u0437\u0430\u0446\u0438\
  \u0438 \u0434\u0430\u043D\u043D\u044B\u0445, \u043F\u043E\u0434\u043E\u0431\u043D\
  \u044B\u0439 JSON \u0438\u043B\u0438 YAML."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 TOML"
weight: 39
---

## Как это сделать:
Сначала вам понадобится парсер TOML. `@iarna/toml` - популярный выбор. Установите его с помощью npm: `npm install @iarna/toml --save`. Вот как вы читаете файл TOML и анализируете его в объект JavaScript:

```typescript
import * as fs from 'fs';
import toml from '@iarna/toml';

const tomlContent = fs.readFileSync('config.toml', 'utf-8');
const parsedData = toml.parse(tomlContent);

console.log(parsedData);
```
Если содержимое `config.toml` будет:
```
[server]
port = 8080
```
Вывод будет:
```
{ server: { port: 8080 } }
```
И запись в файл TOML также проста:
```typescript
import * as fs from 'fs';
import { stringify } from '@iarna/toml';

const obj = { server: { port: 8080 } };
const tomlString = stringify(obj);
fs.writeFileSync('config.toml', tomlString);
``` 
Выполнение этого кода записывает объект в `config.toml` в формате TOML.

## Глубокое погружение
TOML был создан Томом Престон-Вернером, сооснователем GitHub, около 2013 года в ответ на ограничения, которые он видел в других форматах, таких как INI или YAML. Он разработан чтобы быть однозначным и легко анализируемым в структуры данных, поэтому является фаворитом для файлов конфигурации. Альтернативы, такие как JSON, не имеют комментариев, в то время как YAML более сложен. TOML выделяется своей простотой и способностью ясно представлять сложные иерархии данных.

Внутри, когда вы анализируете TOML в TypeScript, вы преобразуете текстовые данные в структурированный формат, который язык может манипулировать. Это включает лексический анализ (превращение необработанного текста в токены) и разбор (построение внутренней структуры данных); `@iarna/toml` обрабатывает оба эти процесса без проблем. Поддержка эмодзи - это весёлое дополнение, показывающее ориентированный на пользователя подход TOML.

## Смотрите также
- Официальная спецификация TOML: https://toml.io/en/
- Пакет `@iarna/toml`: https://www.npmjs.com/package/@iarna/toml
- Сравнения между TOML, YAML и JSON: https://blog.bitsrc.io/choosing-the-right-configuration-file-format-toml-vs-yaml-vs-json-71b5be8968ea
