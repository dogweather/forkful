---
title:                "Робота з json"
html_title:           "Javascript: Робота з json"
simple_title:         "Робота з json"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/working-with-json.md"
---

{{< edit_this_page >}}

## Чому

Робота з JSON (JavaScript Object Notation) є невід'ємною частиною сучасного програмування. Вона дозволяє обмінюватися даними між різними додатками та серверами, що робить її потужним інструментом для розробників.

## Як

Для початку роботи з JSON, вам необхідно створити або отримати даний об'єкт у форматі JSON. Існує кілька способів цього зробити, але найбільш поширений - це використання функції `JSON.parse()` для перетворення рядка з JSON у об'єкт JavaScript.

```Javascript
let jsonString = '{"name": "John", "age": 30, "city": "Kyiv"}'; // рядок з JSON
let person = JSON.parse(jsonString); // перетворення в об'єкт JavaScript
console.log(person.name); // виведе "John"
```

Для створення об'єкта JSON з JavaScript об'єкту використовуйте функцію `JSON.stringify()`, яка перетворить об'єкт у рядок з JSON.

```Javascript
let person = { name: "John", age: 30, city: "Kyiv" }; // об'єкт JavaScript
let jsonString = JSON.stringify(person); // перетворення в рядок JSON
console.log(jsonString); // виведе '{"name": "John", "age": 30, "city": "Kyiv"}'
```

## Глибінний аналіз

JSON дозволяє зберігати дані в простий і зрозумілий формат, що робить його ідеальним для обміну даними між різними системами та пристроями. Крім того, він підтримується більшістю програмних мов та є стандартом у веб-розробці.

Однак, варто пам'ятати про декілька особливостей роботи з JSON. Наприклад, він не може напряму працювати з функціями та датами, тому їх необхідно перетворювати у спеціальні формати, наприклад, до рядків.

## Дивіться також

- [Офіційна документація JSON](https://www.json.org/)
- [JSON Tutorial на w3schools.com](https://www.w3schools.com/js/js_json_intro.asp)
- [JavaScript Object Notation на MDN](https://developer.mozilla.org/uk/docs/Learn/JavaScript/Objects/JSON)