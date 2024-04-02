---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:31.324962-07:00
description: "JSON (JavaScript Object Notation) - \u0446\u0435 \u043B\u0435\u0433\u043A\
  \u0438\u0439 \u0444\u043E\u0440\u043C\u0430\u0442 \u043E\u0431\u043C\u0456\u043D\
  \u0443 \u0434\u0430\u043D\u0438\u043C\u0438, \u044F\u043A\u0438\u0439 \u043B\u0435\
  \u0433\u043A\u043E \u0447\u0438\u0442\u0430\u0454\u0442\u044C\u0441\u044F \u0456\
  \ \u043F\u0438\u0448\u0435\u0442\u044C\u0441\u044F \u043B\u044E\u0434\u0438\u043D\
  \u043E\u044E, \u0430 \u0442\u0430\u043A\u043E\u0436 \u043B\u0435\u0433\u043A\u043E\
  \ \u0430\u043D\u0430\u043B\u0456\u0437\u0443\u0454\u0442\u044C\u0441\u044F \u0456\
  \ \u0433\u0435\u043D\u0435\u0440\u0443\u0454\u0442\u044C\u0441\u044F\u2026"
lastmod: '2024-03-13T22:44:50.030207-06:00'
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) - \u0446\u0435 \u043B\u0435\u0433\u043A\
  \u0438\u0439 \u0444\u043E\u0440\u043C\u0430\u0442 \u043E\u0431\u043C\u0456\u043D\
  \u0443 \u0434\u0430\u043D\u0438\u043C\u0438, \u044F\u043A\u0438\u0439 \u043B\u0435\
  \u0433\u043A\u043E \u0447\u0438\u0442\u0430\u0454\u0442\u044C\u0441\u044F \u0456\
  \ \u043F\u0438\u0448\u0435\u0442\u044C\u0441\u044F \u043B\u044E\u0434\u0438\u043D\
  \u043E\u044E, \u0430 \u0442\u0430\u043A\u043E\u0436 \u043B\u0435\u0433\u043A\u043E\
  \ \u0430\u043D\u0430\u043B\u0456\u0437\u0443\u0454\u0442\u044C\u0441\u044F \u0456\
  \ \u0433\u0435\u043D\u0435\u0440\u0443\u0454\u0442\u044C\u0441\u044F\u2026"
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 JSON"
weight: 38
---

## Що і Чому?

JSON (JavaScript Object Notation) - це легкий формат обміну даними, який легко читається і пишеться людиною, а також легко аналізується і генерується машиною. Програмісти використовують його для зберігання та передачі даних у веб-додатках, що робить його основою сучасного API та спілкування веб-сервісів.

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
