---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:31.324962-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : #."
lastmod: '2024-03-13T22:44:50.030207-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 JSON"
weight: 38
---

## Як це зробити:


### Парсинг JSON
Щоб перетворити рядок JSON у об'єкт JavaScript, використовуйте `JSON.parse()`.

```javascript
const jsonString = '{"name":"John", "age":30, "city":"New York"}';
const obj = JSON.parse(jsonString);
console.log(obj.name); // Вивід: John
```

### Приведення об’єктів JavaScript до рядка JSON
Щоб перетворити об'єкт JavaScript назад у рядок JSON, використовуйте `JSON.stringify()`.

```javascript
const user = { name: "Jane", age: 25, city: "London" };
const jsonString = JSON.stringify(user);
console.log(jsonString); // Вивід: {"name":"Jane","age":25,"city":"London"}
```

### Робота з файлами в Node.js
Щоб прочитати файл JSON і перетворити його на об'єкт у середовищі Node.js, ви можете використати модуль `fs`. Припустимо, у вас є файл під назвою `data.json`.

```javascript
const fs = require('fs');

fs.readFile('data.json', 'utf-8', (err, data) => {
    if (err) throw err;
    const obj = JSON.parse(data);
    console.log(obj);
});
```

Для запису об'єкта у файл JSON:

```javascript
const fs = require('fs');
const user = { name: "Mike", age: 22, city: "Berlin" };

fs.writeFile('user.json', JSON.stringify(user, null, 2), (err) => {
    if (err) throw err;
    console.log('Дані записано у файл');
});
```

### Сторонні бібліотеки
Для складніших операцій з JSON, фреймворки і бібліотеки, як-от `lodash`, можуть спростити завдання, але для базових операцій часто достатньо нативних функцій JavaScript. Для масштабних або критичних за продуктивністю додатків можна розглянути бібліотеки на кшталт `fast-json-stringify` для швидшого перетворення в рядок JSON або `json5` для парсингу та зворотного перетворення за допомогою більш гнучкого формату JSON.

Парсинг за допомогою `json5`:
```javascript
const JSON5 = require('json5');

const jsonString = '{name:"John", age:30, city:"New York"}';
const obj = JSON5.parse(jsonString);
console.log(obj.name); // Вивід: John
```

Ці приклади охоплюють базові операції з JSON у JavaScript, ідеально підходять для початківців, які переходять з інших мов і бажають ефективно працювати з даними у веб-додатках.
