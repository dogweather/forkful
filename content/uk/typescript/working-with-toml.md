---
title:                "Робота з TOML"
date:                  2024-01-26T04:27:46.675305-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/working-with-toml.md"
---

{{< edit_this_page >}}

## Що та Чому?
TOML, що є скороченням від Tom's Obvious, Minimal Language (Явна, Мінімальна Мова Тома), - це формат серіалізації даних на зразок JSON або YAML. Програмісти використовують його через його легкість для сприйняття людиною та пряму відповідність до типів даних, що робить його ідеальним для файлів конфігурації та обміну даними.

## Як це зробити:
Спочатку, вам знадобиться парсер TOML. `@iarna/toml` є популярним варіантом. Встановіть його за допомогою npm: `npm install @iarna/toml --save`. Ось як ви можете прочитати файл TOML та розібрати його до об'єкта JavaScript:

```typescript
import * as fs from 'fs';
import toml from '@iarna/toml';

const tomlContent = fs.readFileSync('config.toml', 'utf-8');
const parsedData = toml.parse(tomlContent);

console.log(parsedData);
```
Якщо `config.toml` містить:
```
[server]
port = 8080
```
Тоді вивід буде таким:
```
{ server: { port: 8080 } }
```
І, запис у файл TOML є так само простим:
```typescript
import * as fs from 'fs';
import { stringify } from '@iarna/toml';

const obj = { server: { port: 8080 } };
const tomlString = stringify(obj);
fs.writeFileSync('config.toml', tomlString);
``` 
Виконання цього коду записує об'єкт у `config.toml` у форматі TOML.

## Поглиблений Розгляд
TOML був створений Томом Престон-Вернером, співзасновником GitHub, приблизно у 2013 році як відповідь на обмеження, які він сприймав у інших форматах, таких як INI або YAML. Він розроблений бути недвозначним і легким для розбору в структури даних, отже, улюбленець для файлів конфігурації. Альтернативи, на зразок JSON, не мають коментарів, тоді як YAML є складнішим. TOML виблискує своєю простотою та здатністю чітко представляти складні ієрархії даних.

Під капотом, коли ви парсите TOML в TypeScript, ви перетворюєте текстові дані в структурований формат, який мова може маніпулювати. Це включає лексинг (перетворення сирого тексту в токени) та парсинг (створення внутрішньої структури даних); `@iarna/toml` впорається з обома безперешкодно. Підтримка емодзі є веселим дотиком, що показує користувацький підхід TOML.

## Див. також
- Офіційна специфікація TOML: https://toml.io/en/
- Пакет `@iarna/toml`: https://www.npmjs.com/package/@iarna/toml
- Порівняння між TOML, YAML та JSON: https://blog.bitsrc.io/choosing-the-right-configuration-file-format-toml-vs-yaml-vs-json-71b5be8968ea
