---
title:                "Javascript: Робота з csv"
simple_title:         "Робота з csv"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

## Чому

Для чого розпочати роботу з CSV? Цей формат даних широко використовується в програмуванні для імпорту і експорту інформації. Робота з CSV дозволяє зручно обмінюватися даними між різними програмами і системами.

## Як це зробити

Найпростішим способом роботи з CSV є використання вбудованого об'єкта `CSV` в Javascript. Для цього потрібно створити новий об'єкт `CSV` та передати йому шлях до файлу CSV у якості параметру. Нижче наведені приклади коду та виведення даних.

```Javascript
// Створення об'єкта CSV з файлу "./data.csv"
const csv = new CSV("./data.csv");

// Отримання масиву рядків з файлу CSV
const lines = csv.getLines();
console.log(lines);
// Виведе масив рядків зі всіма значеннями в кожному рядку

// Отримання масиву об'єктів з файлу CSV, де ключами є перші значення у рядку
const objects = csv.getObjects();
console.log(objects);
// Виведе масив об'єктів зі значеннями в якості властивостей об'єктів
```

## Глибоке дослідження

Щоб більш детально працювати з CSV в Javascript, можна використовувати додаткові бібліотеки, наприклад, `csv-parser` або `fast-csv`. Вони дозволяють виконувати більш складні операції з CSV, такі як фільтрація або маніпулювання даними. Деякі бібліотеки також мають підтримку для асинхронного читання та запису CSV файлів.

## Дивіться також

- [Вбудований об'єкт CSV в Javascript](https://developer.mozilla.org/uk/docs/Web/API/CSV)
- [Бібліотека `csv-parser`](https://www.npmjs.com/package/csv-parser)
- [Бібліотека `fast-csv`](https://www.npmjs.com/package/fast-csv)