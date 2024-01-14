---
title:                "Gleam: Знаходження довжини рядка"
simple_title:         "Знаходження довжини рядка"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Чому

Неодноразово у вас може виникнути необхідність дізнатися довжину текстового рядку, наприклад, для перевірки чи він не перевищує максимально дозволену кількість символів. У цьому випадку корисним інструментом буде використання Gleam, який дозволяє швидко та ефективно знаходити довжину рядку.

## Як це зробити

Щоб отримати довжину рядку в Gleam, використовуйте вбудовану функцію `string.length`. Ось приклад коду з використанням цієї функції:

```Gleam
let string = "Привіт світе"
let length = string.length
```

У даному випадку, змінна `length` буде містити число 12, оскільки у рядку "Привіт світе" 12 символів.

## Поглиблене вивчення

Функція `string.length` є частиною `string` модулю в Gleam, який надає багато інших корисних функцій для роботи з рядками. Наприклад, за допомогою функції `string.slice` можна отримати певний виріз з рядка. Також, модуль містить функцію `string.contains` для перевірки чи містить рядок певний підрядок.

## Дивіться також

- [Документація Gleam](https://gleam.run/documentation/)
- [Основні операції з рядками у Gleam](https://gleam.run/documentation/framework/strings/)
- [Приклади використання рядків у Gleam](https://github.com/gleam-lang/gleam_examples/tree/master/strings)