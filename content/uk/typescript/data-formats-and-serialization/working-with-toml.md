---
date: 2024-01-26 04:27:46.675305-07:00
description: "TOML, \u0449\u043E \u0454 \u0441\u043A\u043E\u0440\u043E\u0447\u0435\
  \u043D\u043D\u044F\u043C \u0432\u0456\u0434 Tom's Obvious, Minimal Language (\u042F\
  \u0432\u043D\u0430, \u041C\u0456\u043D\u0456\u043C\u0430\u043B\u044C\u043D\u0430\
  \ \u041C\u043E\u0432\u0430 \u0422\u043E\u043C\u0430), - \u0446\u0435 \u0444\u043E\
  \u0440\u043C\u0430\u0442 \u0441\u0435\u0440\u0456\u0430\u043B\u0456\u0437\u0430\u0446\
  \u0456\u0457 \u0434\u0430\u043D\u0438\u0445 \u043D\u0430 \u0437\u0440\u0430\u0437\
  \u043E\u043A JSON \u0430\u0431\u043E YAML.\u2026"
lastmod: 2024-02-19 22:05:07.914569
model: gpt-4-0125-preview
summary: "TOML, \u0449\u043E \u0454 \u0441\u043A\u043E\u0440\u043E\u0447\u0435\u043D\
  \u043D\u044F\u043C \u0432\u0456\u0434 Tom's Obvious, Minimal Language (\u042F\u0432\
  \u043D\u0430, \u041C\u0456\u043D\u0456\u043C\u0430\u043B\u044C\u043D\u0430 \u041C\
  \u043E\u0432\u0430 \u0422\u043E\u043C\u0430), - \u0446\u0435 \u0444\u043E\u0440\u043C\
  \u0430\u0442 \u0441\u0435\u0440\u0456\u0430\u043B\u0456\u0437\u0430\u0446\u0456\u0457\
  \ \u0434\u0430\u043D\u0438\u0445 \u043D\u0430 \u0437\u0440\u0430\u0437\u043E\u043A\
  \ JSON \u0430\u0431\u043E YAML.\u2026"
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 TOML"
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
