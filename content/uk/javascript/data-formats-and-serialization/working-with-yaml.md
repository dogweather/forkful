---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:59.457791-07:00
description: "YAML, \u0449\u043E \u0440\u043E\u0437\u0448\u0438\u0444\u0440\u043E\u0432\
  \u0443\u0454\u0442\u044C\u0441\u044F \u044F\u043A YAML Ain't Markup Language (YAML\
  \ \u2014 \u0446\u0435 \u043D\u0435 \u043C\u043E\u0432\u0430 \u0440\u043E\u0437\u043C\
  \u0456\u0442\u043A\u0438), \u2014 \u0446\u0435 \u0444\u043E\u0440\u043C\u0430\u0442\
  \ \u0441\u0435\u0440\u0456\u0430\u043B\u0456\u0437\u0430\u0446\u0456\u0457 \u0434\
  \u0430\u043D\u0438\u0445, \u043F\u0440\u0438\u0434\u0430\u0442\u043D\u0438\u0439\
  \ \u0434\u043B\u044F \u0447\u0438\u0442\u0430\u043D\u043D\u044F \u043B\u044E\u0434\
  \u0438\u043D\u043E\u044E.\u2026"
lastmod: '2024-03-13T22:44:50.028502-06:00'
model: gpt-4-0125-preview
summary: "YAML, \u0449\u043E \u0440\u043E\u0437\u0448\u0438\u0444\u0440\u043E\u0432\
  \u0443\u0454\u0442\u044C\u0441\u044F \u044F\u043A YAML Ain't Markup Language (YAML\
  \ \u2014 \u0446\u0435 \u043D\u0435 \u043C\u043E\u0432\u0430 \u0440\u043E\u0437\u043C\
  \u0456\u0442\u043A\u0438), \u2014 \u0446\u0435 \u0444\u043E\u0440\u043C\u0430\u0442\
  \ \u0441\u0435\u0440\u0456\u0430\u043B\u0456\u0437\u0430\u0446\u0456\u0457 \u0434\
  \u0430\u043D\u0438\u0445, \u043F\u0440\u0438\u0434\u0430\u0442\u043D\u0438\u0439\
  \ \u0434\u043B\u044F \u0447\u0438\u0442\u0430\u043D\u043D\u044F \u043B\u044E\u0434\
  \u0438\u043D\u043E\u044E."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 YAML"
weight: 41
---

## Як користуватися:
У JavaScript робота з YAML, як правило, передбачає використання сторонньої бібліотеки, оскільки мова не має вбудованого парсера для YAML. Однією з найпопулярніших бібліотек для цих цілей є `js-yaml`. Ви можете використовувати `js-yaml` для аналізу YAML у JavaScript об'єкти та навпаки.

Спершу вам потрібно встановити `js-yaml`:

```bash
npm install js-yaml
```

Потім ви можете використовувати його у своїх проєктах. Ось як ви можете завантажити файл YAML і аналізувати його до JavaScript об'єкта:

```javascript
// Підключення модуля js-yaml
const yaml = require('js-yaml');
const fs   = require('fs');

// Завантаження YAML з файлу
try {
  const doc = yaml.load(fs.readFileSync('./config.yaml', 'utf8'));
  console.log(doc);
} catch (e) {
  console.error(e);
}
```

Якщо ваш файл `config.yaml` виглядає так:

```yaml
version: 1
services:
  web:
    image: "myapp/web:latest"
    ports:
      - "5000:5000"
```

Вивід буде:

```javascript
{ version: 1,
  services: 
   { web: 
      { image: 'myapp/web:latest',
        ports: [ '5000:5000' ] } } }
```

Для виконання зворотного перетворення, конвертування JavaScript об'єкта в рядок YAML:

```javascript
const yaml = require('js-yaml');
const obj = {
  version: 1,
  services: {
    web: {
      image: "myapp/web:latest",
      ports: ["5000:5000"]
    }
  }
};

const yamlStr = yaml.dump(obj);
console.log(yamlStr);
```

Цей код створить:

```yaml
version: 1
services:
  web:
    image: myapp/web:latest
    ports:
      - '5000:5000'
```

Використовуючи `js-yaml`, ви можете легко інтегрувати аналіз та серіалізацію YAML у свої JavaScript проєкти, підвищуючи можливості обміну даними та управління конфігурацією.
