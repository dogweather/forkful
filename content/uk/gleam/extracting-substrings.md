---
title:                "Gleam: Видобуття підрядків"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Для чого

Використання функції для видобування підрядків може бути корисним для різних сценаріїв кодування, таких як знаходження певної частини тексту або оброблення великих рядків даних. Це може також зробити взаємодію зі строки більш ефективною та зручною.

## Як це зробити

Ми можемо використовувати функцію для видобування підрядка у Gleam, щоб отримати певну кількість символів з початку або кінця строки або з визначеного місця. Наприклад, у наступному коді ми видобуємо 5 символів з початку строки і отримуємо вихід "Hello":

```Gleam
let text = "Hello, world!"
let substring = String.slice(text, 0, 5)

io.print(substring) // Output: "Hello"
```

Ми також можемо використовувати цю функцію для знаходження певного слова або речення у строкі. Наприклад, у наступному коді ми шукаємо слово "world" у строкі і отримуємо вихід "world!":

```Gleam
let text = "Hello, world!"
let substring = String.search(text, "world")

io.print(substring) // Output: "world!"
```

## Розгляд в глибину

Функція для видобування підрядків у Gleam базується на модулі String, який містить багато корисних функцій для роботи зі строками. У випадку, якщо нам потрібно видобути підрядок, що починається з певного символу або закінчується певним символом, ми можемо використати функції String.left и String.right для видобування потрібної частини строки.

Наприклад, у наступному коді ми шукаємо підрядок, що міститься між символами "!" і ".", і отримуємо вихід "world":

```Gleam
let text = "Hello, world!"
let substring = String.left(String.right(text, 5), 1)

io.print(substring) // Output: "world"
```

## Дивіться також

- [Dive into Gleam strings](https://gleam.run/articles/primitives-strings)
- [Strings module documentation](https://gleam.run/docs/stdlib/strings)