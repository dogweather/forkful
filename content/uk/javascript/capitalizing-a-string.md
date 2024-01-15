---
title:                "Капіталізація рядка"
html_title:           "Javascript: Капіталізація рядка"
simple_title:         "Капіталізація рядка"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Чому

Використання функцій для перетворення перших літер слова у великі - це важлива частина розробки пов'язаної з наданням читабельності і красивого вигляду тексту. Використання цих функцій в JavaScript дозволяє нам швидко і ефективно писати код, який перетворює символи строки у великі літери.

## Як це зробити

```Javascript
let str = "привіт світ!"

// Використання методу toUpperCase() для перетворення першої літери на велику
let capitalizedStr = str.charAt(0).toUpperCase() + str.slice(1);

// Результат: "Привіт світ!"
console.log(capitalizedStr);
```

У цьому прикладі ми використовуємо метод `toUpperCase()` для перетворення першої літери в велику, а потім додаємо зріз строки, щоб додати решту символів. Цей підхід працює не тільки для першої літери, але і для будь-якої літери в слові.

## Глибоке поглиблення

Існує кілька методів у JavaScript для перетворення літер строки у великі. Окрім `toUpperCase()`, ми також можемо використовувати `toLowerCase()` для перетворення літер у нижній регістр, а також `charAt()` і `slice()` для отримання і редагування окремих символів. Для більш складних оперіцій зі строками, таких як перетворення до капіталізації кожного слова, можна використовувати регулярні вирази.

## Дивіться також

- [MDN Документація про метод toUpperCase()](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [MDN Документація про метод toLowerCase()](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [MDN Документація про метод charAt()](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/charAt)
- [MDN Документація про метод slice()](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [MDN Документація про регулярні вирази в JavaScript](https://developer.mozilla.org/uk/docs/Web/JavaScript/Guide/Regular_Expressions)