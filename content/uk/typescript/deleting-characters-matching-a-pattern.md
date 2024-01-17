---
title:                "Видалення символів, що відповідають шаблону"
html_title:           "TypeScript: Видалення символів, що відповідають шаблону"
simple_title:         "Видалення символів, що відповідають шаблону"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

Що & Чому?
Видалення символів, що збігаються зі шаблоном, це процес видалення певної послідовності символів з тексту за допомогою певного шаблону. Програмісти використовують цей процес для видалення непотрібних або небажаних символів зі строки або тексту.

Як це зробити:
```TypeScript
const string = "Hello World!" // задаємо текст
const pattern = /[aeiou]/gi // задаємо шаблон (глобально, без врахування регістру)
const result = string.replace(pattern, "") // видаляємо всі голосні з тексту
console.log(result) // виводимо результат: Hll Wrld!
```

Глибше:
1. Історичний контекст: видалення символів, що збігаються зі шаблоном, завжди було популярним способом обробки тексту для програмістів.
2. Альтернативи: іншим способом видалення символів з тексту є використання циклу та оператора "if", але це може бути більш затратною операцією, особливо для великих текстових даних.
3. Деталі реалізації: у TypeScript видалення символів з тексту можна зробити за допомогою методу `replace()` з використанням регулярних виразів.

Дивись також:
- [RegExp - MDN](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/RegExp)
- [Створення регулярних виразів в TypeScript](https://www.typescriptlang.org/uk/docs/handbook/regular-expressions.html)