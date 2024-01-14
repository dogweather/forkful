---
title:    "Javascript: З'єднання рядків"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Чому

Конкатенація рядків є важливою частиною програмування в Javascript і може бути корисною для об'єднання інформації з різних джерел в один рядок, роблячи код більш ефективним і зрозумілим.

## Як

Для конкатенації рядків в Javascript потрібно використати оператор "+" або метод "concat()". Наприклад:

```Javascript
let firstName = "Анна";
let lastName = "Іваненко";

let fullName = firstName + " " + lastName;
// Результат: Анна Іваненко

let welcomeMessage = "Ласкаво просимо, ";
welcomeMessage = welcomeMessage.concat(firstName, "!");
// Результат: Ласкаво просимо, Анна!
```

## Глибоке занурення

Оператор "+" може бути використаний для конкатенації рядків, чисел та інших типів даних, що автоматично перетворює типи. Крім того, додаткові пробіли можуть бути додані за допомогою методу "trim()". Наприклад:

```Javascript
let num1 = 10;
let num2 = 5;
let result = "Сума чисел " + num1 + " та " + num2 + " дорівнює " + (num1 + num2);
// Результат: Сума чисел 10 та 5 дорівнює 15

let sentence = "  Привіт, я рядок  ";
sentence = sentence.trim();
// Результат: Привіт, я рядок
```

## See Also

- [Оператор "+" в Javascript](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Operators/Concatenation)
- [Метод "concat()" в Javascript](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [Типи даних в Javascript](https://developer.mozilla.org/uk/docs/Web/JavaScript/Data_structures)