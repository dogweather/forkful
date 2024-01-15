---
title:                "Робота з YAML"
html_title:           "TypeScript: Робота з YAML"
simple_title:         "Робота з YAML"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Чому

YAML (Yet Another Markup Language) є популярним форматом для зберігання даних, що зручно використовувати для налаштування та проектування. Цей формат є читабельним для людей та підтримується більшістю програмних мов, включаючи TypeScript. Підручно мати знання про роботу з YAML, коли ви працюєте з конфігураційними файлами або структурованими даними у додатках.

## Як

### Встановлення бібліотеки YAML для TypeScript
Щоб працювати з YAML в TypeScript, вам потрібно встановити бібліотеку, наприклад `js-yaml`. Зробити це можна за допомогою менеджера пакетів npm, виконавши команду `npm install js-yaml` у терміналі вашого проекту.

### Читання та запис YML файлів
Після встановлення бібліотеки `js-yaml`, ви можете почати читати та записувати файли YML. Нижче наведено приклад коду, який читає YML файл та виводить його зміст у консоль:

```TypeScript
const yaml = require('js-yaml');
const fs = require('fs');

try {
  const fileData = fs.readFileSync('config.yml', 'utf8');
  console.log(yaml.safeLoad(fileData));
} catch (error) {
  console.log(error);
}
```

### Парсинг рядків YAML
Часто YML дані приходять у вигляді рядків, і необхідно їх перетворити у об'єкти для подальшої роботи з ними. Для цього використовуйте метод `yaml.safeLoad()`, передавши йому рядок крім змінної `fileData`.

```TypeScript
const yaml = require('js-yaml');
const configString = "name: John\nage: 25";
const configObject = yaml.safeLoad(configString);
console.log(configObject.name); // виведе "John"
```

## Глибокий занурення

- Якщо ви працюєте з багатомовними даними, YAML має вбудовану підтримку для виведення даних у різні мови.
- При використанні бібліотеки `js-yaml`, є можливість обробляти помилки під час парсингу файлу YAML та опрацьовувати винятки.
- YAML також підтримує об'єктные теги, що дозволяють вставляти JavaScript функції або об'єкти у YML документи.

## Дивись також
- [Документація по YAML](https://yaml.org/)
- [Бібліотека js-yaml](https://github.com/nodeca/js-yaml) для роботи з YAML у JavaScript/TypeScript.