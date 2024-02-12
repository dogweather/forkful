---
title:                "Робота з YAML"
aliases:
- /uk/javascript/working-with-yaml.md
date:                  2024-02-03T19:25:59.457791-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та чому?

YAML, що розшифровується як YAML Ain't Markup Language (YAML — це не мова розмітки), — це формат серіалізації даних, придатний для читання людиною. Програмісти часто використовують його для файлів конфігурації та обміну даними між мовами завдяки його простоті та зручності порівняно із JSON або XML.

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
