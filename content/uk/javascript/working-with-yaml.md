---
title:                "Javascript: Робота з yaml"
simple_title:         "Робота з yaml"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Чому

Програмування на Javascript є важливою навичкою для будь-якого розробника, але якщо ви займаєтеся веб-розробкою, це важливо в двічі. І саме тоді, коли ви знаходитеся в середовищі веб-розробки, ви зіштовхуєтесь з багатою кількістю форматів файлів, одним з яких є YAML.

YAML (або "YAML Ain't Markup Language") є простим та зрозумілим форматом, що використовується для збереження даних у вигляді текстового файлу. Ви можете використовувати його для збереження налаштувань, даних конфігурації та багатьох інших речей. В цілому, це можливості, які роблять YAML дуже корисним для веб-розробників.

## Як

Якщо ви ще не маєте досвіду з роботою з YAML, це зовсім не проблема. В цій статті ми розглянемо, як працювати з YAML в Javascript та як його застосовувати для збереження даних у вашій програмі.

Спочатку, ми використовуємо модуль npm (Node Package Manager) для встановлення бібліотеки js-yaml, яка надає нам можливість працювати з YAML у Javascript. Після встановлення ми можемо імпортувати цей модуль у наш проект.

```
npm install js-yaml
```

Тепер, коли ми маємо бібліотеку js-yaml, ми можемо почати робити речі з YAML. Наприклад, ми можемо зберегти об'єкт Javascript у файл YAML, використовуючи функцію dump.

```
const yaml = require('js-yaml');

let myObj = {
  name: 'John',
  age: 27,
  hobbies: ['coding', 'hiking', 'reading']
};

const yamlString = yaml.dump(myObj);

console.log(yamlString);

/* Вивід:
name: John
age: 27
hobbies:
  - coding
  - hiking
  - reading
 */
```

Ми також можемо зберегти наш об'єкт у файл YAML, використовуючи функцію safeDump. Це дозволяє нам зберегти дані у вигляді текстового файлу, що є більш зручним для подальшої обробки.

```
yaml.safeDump(myObj, 'myFile.yaml');
```

Щоб отримати об'єкт Javascript з файлу YAML, ми можемо використовувати функцію load або safeLoad.

```
const fs = require('fs');

let yamlString = fs.readFileSync('myFile.yaml', 'utf-8');

let myObj = yaml.load(yamlString);

console.log(myObj);

/* Вивід:
{
  name: 'John',
  age: 27,
  hobbies: ['coding', 'hiking', 'reading']
}
*/
```

Для легшого доступу до даних у нашому об'єкті, ми також можемо використовувати функці