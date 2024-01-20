---
title:                "Перетворення рядка в нижній регістр"
html_title:           "Elixir: Перетворення рядка в нижній регістр"
simple_title:         "Перетворення рядка в нижній регістр"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Що та чому?
Конвертація рядка в нижній регістр - це перетворення всіх символів рядка з верхнього регістра на нижній. Це важлива задача для програмістів, адже дозволяє враховувати введені користувачем дані без врахування регістру символів.

## Як це зробити:

```TypeScript
let stringExample: string = "JavaScript і TypeScript РОЗБІЖНЯЮТЬСЯ";
console.log(stringExample.toLowerCase());
```
Виведення:

```TypeScript
"javascript і typescript розбіжняються"
```

## Поглиблений огляд

1.  Історичний контекст: З моменту створення перших мов програмування функція переведення рядка в нижній регістр використовувалась постійно. Вона важлива для розрізнення маленьких і великих букв, котрі в багатьох мовах мають різне значення.
   
2.  Альтернативи: Деякі інші мови, як-от Javascript, використовують `toLowerCase()` так само, як TypeScript. Інші мови, як-от Python, використовують `lower()`.
   
3.  Деталі реалізації: В TypeScript, `toLowerCase()` є методом об'єкта String, який повертає копію початкового рядка, але з усіма великими буквами заміненими на маленькі. Ця функція не враховує специфіку мови, тому у деяких випадках краще використовувати `toLocaleLowerCase()`.
   
## Дивіться також

- [MDN документація для toLowerCase()](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [Документація по TypeScript](https://www.typescriptlang.org/docs/)