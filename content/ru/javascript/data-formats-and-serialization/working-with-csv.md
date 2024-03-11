---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:23.747429-07:00
description: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 CSV (\u0437\u043D\u0430\u0447\
  \u0435\u043D\u0438\u044F, \u0440\u0430\u0437\u0434\u0435\u043B\u0451\u043D\u043D\
  \u044B\u0435 \u0437\u0430\u043F\u044F\u0442\u044B\u043C\u0438) \u0432 JavaScript\
  \ \u043E\u0431\u044B\u0447\u043D\u043E \u043E\u0437\u043D\u0430\u0447\u0430\u0435\
  \u0442 \u0430\u043D\u0430\u043B\u0438\u0437 \u0438 \u0433\u0435\u043D\u0435\u0440\
  \u0438\u0440\u043E\u0432\u0430\u043D\u0438\u0435 \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u044B\u0445 \u0434\u0430\u043D\u043D\u044B\u0445 \u0434\u043B\u044F \u0442\
  \u0430\u0431\u043B\u0438\u0446 \u0438\u043B\u0438 \u043F\u0435\u0440\u0435\u0434\
  \u0430\u0447\u0438 \u0434\u0430\u043D\u043D\u044B\u0445.\u2026"
lastmod: '2024-03-11T00:14:19.444839-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 CSV (\u0437\u043D\u0430\u0447\
  \u0435\u043D\u0438\u044F, \u0440\u0430\u0437\u0434\u0435\u043B\u0451\u043D\u043D\
  \u044B\u0435 \u0437\u0430\u043F\u044F\u0442\u044B\u043C\u0438) \u0432 JavaScript\
  \ \u043E\u0431\u044B\u0447\u043D\u043E \u043E\u0437\u043D\u0430\u0447\u0430\u0435\
  \u0442 \u0430\u043D\u0430\u043B\u0438\u0437 \u0438 \u0433\u0435\u043D\u0435\u0440\
  \u0438\u0440\u043E\u0432\u0430\u043D\u0438\u0435 \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u044B\u0445 \u0434\u0430\u043D\u043D\u044B\u0445 \u0434\u043B\u044F \u0442\
  \u0430\u0431\u043B\u0438\u0446 \u0438\u043B\u0438 \u043F\u0435\u0440\u0435\u0434\
  \u0430\u0447\u0438 \u0434\u0430\u043D\u043D\u044B\u0445.\u2026"
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 CSV"
---

{{< edit_this_page >}}

## Что и почему?
Работа с CSV (значения, разделённые запятыми) в JavaScript обычно означает анализ и генерирование текстовых данных для таблиц или передачи данных. Программисты используют CSV, потому что он очень распространён, лёгок и прост в чтении или создании.

## Как это сделать:

**Преобразование CSV в JSON:**
```javascript
const csv = `name, age, city
Alice, 30, New York
Bob, 22, Los Angeles`;

function csvToJson(csv) {
  const lines = csv.split("\n");
  const headers = lines[0].split(",");
  return lines.slice(1).map(line => {
    const data = line.split(",");
    return headers.reduce((obj, nextKey, index) => {
      obj[nextKey] = data[index];
      return obj;
    }, {});
  });
}

console.log(csvToJson(csv));
// Вывод: [{name: 'Alice', age: '30', city: 'New York'}, {name: 'Bob', age: '22', city: 'Los Angeles'}]
```

**Генерирование CSV из JSON:**
```javascript
const jsonData = [
  { name: "Alice", age: 30, city: "New York" },
  { name: "Bob", age: 22, city: "Los Angeles" }
];

function jsonToCsv(json) {
  const headers = Object.keys(json[0]).join(",");
  const rows = json.map(obj =>
    Object.values(obj).join(",")
  ).join("\n");
  return `${headers}\n${rows}`;
}

console.log(jsonToCsv(jsonData));
// Вывод: name,age,city
//        Alice,30,New York
//        Bob,22,Los Angeles
```

## Подробное погружение

CSV существует с ранних дней вычислительной техники - его легко обрабатывать машинам и понимать людям. Но он не идеален. Если ваши данные сложные или вложенные, JSON или XML могут подойти лучше. С точки зрения реализации, обработка CSV в JavaScript требовала обходных путей из-за отсутствия стандартной библиотеки для этого; однако сегодня множество библиотек, таких как PapaParse или csv-parser, упрощают эту задачу. Также, особые случаи, такие как символы новой строки внутри полей и кодирование символов, могут усложнить обработку CSV и требуют внимательного программирования.

## Смотрите также

- MDN Web Docs о Fetch API: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch (Получение данных CSV из сети)
- PapaParse: https://www.papaparse.com/ (Надежный CSV парсер для браузера)
- RFC 4180: https://tools.ietf.org/html/rfc4180 (Стандарты для файлов CSV)
