---
title:                "Робота з YAML"
aliases:
- /uk/typescript/working-with-yaml.md
date:                  2024-02-03T19:27:19.796317-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
