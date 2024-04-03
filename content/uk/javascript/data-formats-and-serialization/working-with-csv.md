---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:44.963849-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : JavaScript \u043D\u0435 \u043C\u0430\u0454 \u0432\u0431\u0443\u0434\u043E\u0432\
  \u0430\u043D\u043E\u0457 \u0444\u0443\u043D\u043A\u0446\u0456\u043E\u043D\u0430\u043B\
  \u044C\u043D\u043E\u0441\u0442\u0456 \u0434\u043B\u044F \u0430\u043D\u0430\u043B\
  \u0456\u0437\u0443 \u0430\u0431\u043E \u0441\u0442\u0432\u043E\u0440\u0435\u043D\
  \u043D\u044F CSV, \u043D\u0430 \u0432\u0456\u0434\u043C\u0456\u043D\u0443 \u0432\
  \u0456\u0434 JSON. \u041F\u0440\u043E\u0442\u0435 \u0432\u0438 \u043B\u0435\u0433\
  \u043A\u043E \u043C\u043E\u0436\u0435\u0442\u0435 \u043A\u0435\u0440\u0443\u0432\
  \u0430\u0442\u0438 \u0434\u0430\u043D\u0438\u043C\u0438\u2026"
lastmod: '2024-03-13T22:44:50.031811-06:00'
model: gpt-4-0125-preview
summary: "JavaScript \u043D\u0435 \u043C\u0430\u0454 \u0432\u0431\u0443\u0434\u043E\
  \u0432\u0430\u043D\u043E\u0457 \u0444\u0443\u043D\u043A\u0446\u0456\u043E\u043D\u0430\
  \u043B\u044C\u043D\u043E\u0441\u0442\u0456 \u0434\u043B\u044F \u0430\u043D\u0430\
  \u043B\u0456\u0437\u0443 \u0430\u0431\u043E \u0441\u0442\u0432\u043E\u0440\u0435\
  \u043D\u043D\u044F CSV, \u043D\u0430 \u0432\u0456\u0434\u043C\u0456\u043D\u0443\
  \ \u0432\u0456\u0434 JSON."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 CSV"
weight: 37
---

## Як це зробити:
JavaScript не має вбудованої функціональності для аналізу або створення CSV, на відміну від JSON. Проте ви легко можете керувати даними CSV, використовуючи або сирий JavaScript для простіших задач, або вдаючись до потужних бібліотек, як-от `PapaParse`, для більш складних сценаріїв.

### Базовий аналіз за допомогою сирого JavaScript
Для аналізу простого рядка CSV у масив об'єктів:

```javascript
const csv = `name,age,city
John,23,New York
Jane,28,Los Angeles`;

function parseCSV(csv) {
  const lines = csv.split("\n");
  const result = [];
  const headers = lines[0].split(",");

  for (let i = 1; i < lines.length; i++) {
    const obj = {};
    const currentline = lines[i].split(",");
    
    for (let j = 0; j < headers.length; j++) {
      obj[headers[j]] = currentline[j];
    }
    result.push(obj);
  }
  
  return result;
}

console.log(parseCSV(csv));
```
Вивід:

```
[
  { name: 'John', age: '23', city: 'New York' },
  { name: 'Jane', age: '28', city: 'Los Angeles' }
]
```

### Базова генерація до CSV за допомогою сирого JavaScript
Для перетворення масиву об'єктів на рядок CSV:

```javascript
const data = [
  { name: 'John', age: 23, city: 'New York' },
  { name: 'Jane', age: 28, city: 'Los Angeles' }
];

function arrayToCSV(arr) {
  const csv = arr.map(row => 
    Object.values(row).join(',')
  ).join('\n');
  
  return csv;
}

console.log(arrayToCSV(data));
```

Вивід:

```
John,23,New York
Jane,28,Los Angeles
```

### Використання PapaParse для складних завдань з CSV
Для більш складних сценаріїв `PapaParse` є надійною бібліотекою, придатною для аналізу та формування файлів CSV з опціями для потоків, воркерів і обробки великих файлів.

Аналіз файлу CSV або рядка з використанням PapaParse:

```javascript
// Після додавання PapaParse до вашого проекту
const Papa = require('papaparse');
const csv = `name,age,city
John,23,New York
Jane,28,Los Angeles`;

Papa.parse(csv, {
  complete: function(results) {
    console.log("Розібрано:", results.data);
  }
});
```

Генерує:

```
Розібрано: [
  ["name", "age", "city"],
  ["John", "23", "New York"],
  ["Jane", "28", "Los Angeles"]
]
```

Створення рядка CSV з масиву з використанням PapaParse:

```javascript
const data = [
  { name: 'John', age: 23, city: 'New York' },
  { name: 'Jane', age: 28, city: 'Los Angeles' }
];

console.log(Papa.unparse(data));
```

Генерує:

```
name,age,city
John,23,New York
Jane,28,Los Angeles
```

Ці приклади ілюструють базове та розширене управління CSV у JavaScript, спрощуючи обмін даними в веб-додатках і не тільки.
