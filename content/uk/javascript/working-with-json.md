---
title:                "Робота з JSON"
date:                  2024-01-19
simple_title:         "Робота з JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/working-with-json.md"
---

{{< edit_this_page >}}

## Що це таке і навіщо?
JavaScript Object Notation (JSON) - це легкий формат обміну даними. Програмісти використовують його через простоту читання та написання даних, а також легку інтеграцію з мережами.

## Як це зробити:
У наступних прикладах показано, як працювати з JSON у JavaScript:

```javascript
// Парсинг JSON рядка
let jsonData = '{"name": "Oleksiy", "age": 30}';
let user = JSON.parse(jsonData);
console.log(user.name); // Виведе: Oleksiy

// Перетворення об'єкта в JSON рядок
let userObject = { name: "Iryna", age: 25 };
let jsonString = JSON.stringify(userObject);
console.log(jsonString); // Виведе: {"name":"Iryna","age":25}
```

## Поглиблене вивчення:
JSON виник у 2001 році як альтернатива XML. На відміну від XML, він не містить тагів, що робить його менш об'ємним та швидшим для аналізу. Крім того, більшість мов програмування підтримують JSON нативно або через бібліотеки. Fetch API та XMLHttpRequest - приклади інструментів, що працюють з JSON у веб-програмуванні.

## Дивіться також:
- [MDN JSON Documentation](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/JSON) - всебічний ресурс по JSON від Mozilla.
- [JSON.org](https://www.json.org/json-en.html) - основи JSON, з ілюстраціями та прикладами.
- [ECMA-404 The JSON Data Interchange Standard](https://www.ecma-international.org/publications-and-standards/standards/ecma-404/) - офіційний стандарт JSON.
