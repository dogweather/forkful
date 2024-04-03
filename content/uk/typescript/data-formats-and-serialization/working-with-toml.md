---
date: 2024-01-26 04:27:46.675305-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0421\u043F\u043E\u0447\u0430\u0442\u043A\u0443, \u0432\u0430\u043C \u0437\u043D\
  \u0430\u0434\u043E\u0431\u0438\u0442\u044C\u0441\u044F \u043F\u0430\u0440\u0441\u0435\
  \u0440 TOML. `@iarna/toml` \u0454 \u043F\u043E\u043F\u0443\u043B\u044F\u0440\u043D\
  \u0438\u043C \u0432\u0430\u0440\u0456\u0430\u043D\u0442\u043E\u043C. \u0412\u0441\
  \u0442\u0430\u043D\u043E\u0432\u0456\u0442\u044C \u0439\u043E\u0433\u043E \u0437\
  \u0430 \u0434\u043E\u043F\u043E\u043C\u043E\u0433\u043E\u044E npm: `npm install\
  \ @iarna/toml\u2026"
lastmod: '2024-03-13T22:44:48.905347-06:00'
model: gpt-4-0125-preview
summary: "\u0421\u043F\u043E\u0447\u0430\u0442\u043A\u0443, \u0432\u0430\u043C \u0437\
  \u043D\u0430\u0434\u043E\u0431\u0438\u0442\u044C\u0441\u044F \u043F\u0430\u0440\u0441\
  \u0435\u0440 TOML."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 TOML"
weight: 39
---

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
