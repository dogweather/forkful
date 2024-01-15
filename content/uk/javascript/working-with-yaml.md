---
title:                "Робота з yaml"
html_title:           "Javascript: Робота з yaml"
simple_title:         "Робота з yaml"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

# Чому

Якщо ви працюєте з данними у форматі YAML, це може бути цікавим і корисним для створення універсальних та легко зчитуваних файлів конфігурації. Оскільки YAML використовує просте та зрозуміле синтаксис, він є популярним вибором для зберігання налаштувань програм та структурованих даних.

# Як працювати з YAML

Для початку, вам потрібно інсталювати бібліотеку js-yaml, яку можна знайти на сайті npm. Далі, ви можете використовувати функції бібліотеки для парсингу та серіалізації даних у форматі YAML у об'єкти JavaScript та навпаки.

```javascript
const yaml = require('js-yaml');

//  створення об'єкту у форматі YAML
const dataObject = {
  name: "John",
  age: 28
}
const dataYAML = yaml.dump(dataObject);

// перетворення YAML в об'єкт JavaScript
const object = yaml.load(dataYAML);
console.log(object.name); // виводиться "John"
```

# Вивчення YAML ближче

За більш детальною інформацією та прикладами роботи з YAML у проектах, ви можете ознайомитися з його офіційною документацією на сайті yaml.org. Також, варто дослідити додаткові функції та можливості бібліотеки js-yaml, щоб максимально ефективно використовувати данний формат у своїх проектах.

# Дивіться також

- [Офіційна документація YAML](https://yaml.org)
- [Бібліотека js-yaml на npm](https://www.npmjs.com/package/js-yaml)