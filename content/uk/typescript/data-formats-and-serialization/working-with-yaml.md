---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:19.796317-07:00
description: "\u042F\u043A \u043F\u0440\u0430\u0446\u044E\u0432\u0430\u0442\u0438\
  : \u0420\u043E\u0431\u043E\u0442\u0430 \u0437 YAML \u0443 TypeScript, \u044F\u043A\
  \ \u043F\u0440\u0430\u0432\u0438\u043B\u043E, \u043F\u0435\u0440\u0435\u0434\u0431\
  \u0430\u0447\u0430\u0454 \u043F\u0430\u0440\u0441\u0438\u043D\u0433 \u0432\u043C\
  \u0456\u0441\u0442\u0443 YAML \u0443 \u043E\u0431\u2019\u0454\u043A\u0442\u0438\
  \ JavaScript \u0442\u0430 \u043C\u043E\u0436\u043B\u0438\u0432\u0435 \u043F\u043E\
  \u0432\u0435\u0440\u043D\u0435\u043D\u043D\u044F \u043E\u0431\u2019\u0454\u043A\u0442\
  \u0456\u0432 JavaScript \u043D\u0430\u0437\u0430\u0434\u2026"
lastmod: '2024-03-13T22:44:48.900731-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 YAML \u0443 TypeScript, \u044F\
  \u043A \u043F\u0440\u0430\u0432\u0438\u043B\u043E, \u043F\u0435\u0440\u0435\u0434\
  \u0431\u0430\u0447\u0430\u0454 \u043F\u0430\u0440\u0441\u0438\u043D\u0433 \u0432\
  \u043C\u0456\u0441\u0442\u0443 YAML \u0443 \u043E\u0431\u2019\u0454\u043A\u0442\u0438\
  \ JavaScript \u0442\u0430 \u043C\u043E\u0436\u043B\u0438\u0432\u0435 \u043F\u043E\
  \u0432\u0435\u0440\u043D\u0435\u043D\u043D\u044F \u043E\u0431\u2019\u0454\u043A\u0442\
  \u0456\u0432 JavaScript \u043D\u0430\u0437\u0430\u0434 \u0443 YAML."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 YAML"
weight: 41
---

## Як працювати:
Робота з YAML у TypeScript, як правило, передбачає парсинг вмісту YAML у об’єкти JavaScript та можливе повернення об’єктів JavaScript назад у YAML. Це вимагає парсера; одним з популярних варіантів є `js-yaml`, бібліотека, яка легко інтегрується в проєкти TypeScript.

### Встановлення js-yaml
Спочатку додайте `js-yaml` до свого проекту:

```bash
npm install js-yaml
```

### Парсинг YAML у Об’єкт JavaScript
Уявіть, що у вас є YAML файл `config.yaml` з наступним вмістом:

```yaml
database:
  host: localhost
  port: 5432
  username: user
  password: pass
```

Ви можете прочитати та парсити цей файл у об’єкт JavaScript наступним чином:

```typescript
import * as fs from 'fs';
import * as yaml from 'js-yaml';

// Завантаження та парсинг YAML файлу
const fileContents = fs.readFileSync('./config.yaml', 'utf8');
const data = yaml.load(fileContents) as Record<string, any>;

console.log(data);
```

**Приклад Виводу:**

```json
{
  "database": {
    "host": "localhost",
    "port": 5432,
    "username": "user",
    "password": "pass"
  }
}
```

### Конвертація Об’єкта JavaScript в YAML
Якщо вам потрібно зробити навпаки і конвертувати об’єкт JavaScript у строку YAML, ви можете використати `js-yaml` наступним чином:

```typescript
import * as yaml from 'js-yaml';

const obj = {
  title: "Example",
  is_published: true,
  author: {
    name: "Jane Doe",
    age: 34
  }
};

const yamlStr = yaml.dump(obj);
console.log(yamlStr);
```

**Приклад Виводу:**

```yaml
title: Example
is_published: true
author:
  name: Jane Doe
  age: 34
```

Цей уривок конвертує об'єкт JavaScript у строку YAML та виводить її. На практиці ви могли б записати це назад у файл або використати в інших частинах вашого застосунку.
