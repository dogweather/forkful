---
title:                "Работа с TOML"
aliases:
- ru/typescript/working-with-toml.md
date:                  2024-01-29T00:05:26.376958-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/typescript/working-with-toml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
TOML, что расшифровывается как Tom's Obvious, Minimal Language (Простой и Минималистичный Язык Тома), - это формат сериализации данных, подобный JSON или YAML. Программисты используют его за его читаемость для человека и прямолинейное отображение в типы данных, что делает его идеальным выбором для файлов конфигурации и обмена данными.

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
