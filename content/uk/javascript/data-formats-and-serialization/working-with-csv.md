---
title:                "Робота з CSV"
aliases: - /uk/javascript/working-with-csv.md
date:                  2024-02-03T19:20:44.963849-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?
Робота з CSV (Comma-Separated Values — значення, розділені комами) у JavaScript передбачає аналіз або генерацію файлів CSV для того, щоб імпортувати табличні дані з зовнішніх джерел або експортувати дані для використання в інших програмах. Програмісти роблять це тому, що це дозволяє легко та з мінімальними витратами обмінюватися даними між програмами, базами даних і системами, де більш складні формати, наприклад, JSON, можуть бути надлишковими.

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
