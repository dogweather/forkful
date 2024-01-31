---
title:                "Робота з CSV файлами"
date:                  2024-01-19
simple_title:         "Робота з CSV файлами"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
"## Що та Чому?"

Працювати з CSV - це означає читати, писати і маніпулювати даними у форматі Comma-Separated Values. Програмісти це роблять, бо CSV - це простий і широко підтримуваний формат обміну даними, зручний для табличних даних.

## How to:
"## Як це зробити:"

```javascript
// Парсинг CSV рядка в масив
const csv = require('csv-parser');
const fs = require('fs');
const results = [];

fs.createReadStream('data.csv')
  .pipe(csv())
  .on('data', (data) => results.push(data))
  .on('end', () => {
    console.log(results);
    // Працюємо з отриманими даними
  });

// Генерація CSV з масиву об'єктів
const { Parser } = require('json2csv');
const myData = [
  { name: 'Andriy', age: 30 },
  { name: 'Yulia', age: 28 }
];

const json2csvParser = new Parser();
const csv = json2csvParser.parse(myData);

console.log(csv);
```

## Deep Dive
"## Поглиблений Аналіз:"

CSV з'явився у ранніх роках програмування як метод організації та обміну табличними даними. Хоча JSON і XML пропонують більш структуровані формати, CSV залишається популярним через свою простоту і читабельність.

Окрім `csv-parser` і `json2csv`, існують інші бібліотеки як `PapaParse`, `csv-writer`. Вибір бібліотеки залежить від конкретних потреб проекту, роботи зі складними CSV або простотою API.

Зверніть увагу, що коли ми працюємо з CSV у Node.js, потрібно користуватися модулем потоків (streams), щоб ефективно обробляти великі обсяги даних.

## See Also
"## Дивіться Також:"

- [RFC 4180, “Common Format and MIME Type for Comma-Separated Values (CSV) Files”](https://tools.ietf.org/html/rfc4180)
- [PapaParse - Powerful CSV Parser for JavaScript](https://www.papaparse.com/)
- [csv-writer - CSV Writing for Node.js](https://github.com/ryu1kn/csv-writer)
