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

## Що і зачому?
Робота з JSON це швидкий і простий спосіб обміну даними між веб-сервером та клієнтом. Цей формат дозволяє програмістам легко перетворювати дані у JavaScript об’єкти та зворотньо.

## Як це зробити:
Для початку, ви можете створити об’єкт JSON, використовуючи функцію `JSON.stringify()`. Наприклад:
```Javascript
const car = {
  brand: "Tesla",
  model: "Model 3",
  year: 2021,
  color: "black"
};

const json = JSON.stringify(car);

console.log(json); // output: {"brand":"Tesla","model":"Model 3","year":2021,"color":"black"}
```
Щоб перетворити JSON назад у об’єкт JavaScript, використовуйте функцію `JSON.parse()`. Наприклад:
```Javascript
const jsonObj = '{"brand":"Tesla","model":"Model 3","year":2021,"color":"black"}';

const car = JSON.parse(jsonObj);

console.log(car); // output: {brand: "Tesla", model: "Model 3", year: 2021, color: "black"}
```

## Поглиблене вивчення:
JSON був створений в 2001 році та став популярним форматом для передачі даних у світі Веб. Існують інші альтернативи, такі як XML, але JSON став вибором програмістів завдяки своїй простоті та широкій підтримці. У JavaScript, є також функція `JSON.stringify()` та `JSON.parse()` для роботи з цим форматом даних.

## Дивись також:
- [JSON офіційний сайт](https://www.json.org/json-uk.html)
- [Курс "Віртуальний Планета": Робота з JSON](https://www.virtualplanet.org.ua/training/programming/7325)
- [Редактор JSON онлайн](https://jsoneditoronline.org/)