---
title:    "Javascript: Перетворення рядка на рядок з малими літерами"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

##Чому

Перетворення рядка в нижній регістр є важливою функцією в програмуванні. Воно дозволяє зрівняти рядки без врахування регістру і полегшує пошук, фільтрацію та маніпуляцію з даними.

##Як

```Javascript
// Приклад коду для перетворення рядка в нижній регістр
let text = "Привіт, Світе!";
console.log(text.toLowerCase());

// Вивід: привіт, світе!
```
При використанні методу `toLowerCase()`, всі букви в рядку будуть перетворені в нижній регістр. Це дозволить точне порівняння з іншими рядками незалежно від того, які букви були використані. Також ця функція корисна при фільтрації даних або збереженні однакових слів в базі даних.

```Javascript
// Приклад коду для порівняння двох рядків
let firstString = "Привіт, Світе!";
let secondString = "привіт, світе!";
console.log(firstString.toLowerCase() === secondString.toLowerCase());

// Вивід: true
```

## Глибоке дослідження

В JavaScript існує кілька інших методів для перетворення рядка в нижній регістр, таких як `toLowerCase()`, `toLocaleLowerCase()` та `String.fromCharCode()`. Кожен з них має свої особливості і варто ознайомитися з ними, щоб детальніше зрозуміти як працює ця функція.

Наприклад, метод `toLocaleLowerCase()` використовується для перетворення рядка в нижній регістр з урахуванням локалі. Це важливо для міжнародних проектів, де мови використовують різні алфавіти та правила написання.

## Дивіться також

- [MDN документація про toLowerCase()](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [Стаття про працю з рядками в JavaScript](https://www.w3schools.com/jsref/jsref_obj_string.asp)
- [Відеоурок про методи строки в JavaScript](https://www.youtube.com/watch?v=hS_XsZbaMVE)