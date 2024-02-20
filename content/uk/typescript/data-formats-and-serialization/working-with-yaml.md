---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:19.796317-07:00
description: "YAML \u2013 \u0446\u0435 \u043C\u043E\u0432\u0430 \u0441\u0435\u0440\
  \u0456\u0430\u043B\u0456\u0437\u0430\u0446\u0456\u0457 \u0434\u0430\u043D\u0438\u0445\
  , \u044F\u043A\u0430 \u0431\u0443\u043B\u0430 \u0440\u043E\u0437\u0440\u043E\u0431\
  \u043B\u0435\u043D\u0430 \u0442\u0430\u043A\u0438\u043C \u0447\u0438\u043D\u043E\
  \u043C, \u0430\u0431\u0438 \u0431\u0443\u0442\u0438 \u0434\u0440\u0443\u0436\u043D\
  \u044C\u043E\u044E \u0434\u043E \u043B\u044E\u0434\u0438\u043D\u0438, \u0447\u0430\
  \u0441\u0442\u043E \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\
  \u0454\u0442\u044C\u0441\u044F \u0434\u043B\u044F \u0444\u0430\u0439\u043B\u0456\
  \u0432 \u043A\u043E\u043D\u0444\u0456\u0433\u0443\u0440\u0430\u0446\u0456\u0457\
  ,\u2026"
lastmod: 2024-02-19 22:05:07.909548
model: gpt-4-0125-preview
summary: "YAML \u2013 \u0446\u0435 \u043C\u043E\u0432\u0430 \u0441\u0435\u0440\u0456\
  \u0430\u043B\u0456\u0437\u0430\u0446\u0456\u0457 \u0434\u0430\u043D\u0438\u0445\
  , \u044F\u043A\u0430 \u0431\u0443\u043B\u0430 \u0440\u043E\u0437\u0440\u043E\u0431\
  \u043B\u0435\u043D\u0430 \u0442\u0430\u043A\u0438\u043C \u0447\u0438\u043D\u043E\
  \u043C, \u0430\u0431\u0438 \u0431\u0443\u0442\u0438 \u0434\u0440\u0443\u0436\u043D\
  \u044C\u043E\u044E \u0434\u043E \u043B\u044E\u0434\u0438\u043D\u0438, \u0447\u0430\
  \u0441\u0442\u043E \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\
  \u0454\u0442\u044C\u0441\u044F \u0434\u043B\u044F \u0444\u0430\u0439\u043B\u0456\
  \u0432 \u043A\u043E\u043D\u0444\u0456\u0433\u0443\u0440\u0430\u0446\u0456\u0457\
  ,\u2026"
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 YAML"
---

{{< edit_this_page >}}

## Що та Чому?
YAML – це мова серіалізації даних, яка була розроблена таким чином, аби бути дружньою до людини, часто використовується для файлів конфігурації, міжпроцесного обміну повідомленнями та зберігання даних. Програмісти вважають за краще YAML завдяки його читабельності та легкості використання, особливо при роботі зі складними структурованими даними, що робить його відмінним варіантом для застосунків, розроблених на TypeScript.

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
