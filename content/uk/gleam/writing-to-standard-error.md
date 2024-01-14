---
title:    "Gleam: Запис до стандартного помилок"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ПричинаЗапис на стандартну помилку є важливим кроком в програмуванні. Це допомагає відладати код, а також показує помилки, які можуть виникати.  ## ЯкЗапис на стандартну помилку в Gleam легко і просто. Наприклад: ```Gleam -import gleam/io -import gleam/option pub fn main() { let result = io.stderr("Помилка!"); ``` Результат: `error: Помилка!`  ## Глибока заануруванняЗапис на стандартну помилку - це чудовий спосіб для перевірки певних умов у вашому коді. Ви можете використовувати цей підхід для відстеження помилок або для відладки складних проблем. Також ви можете використовувати функцію `io.stderr_silenced()` для того, щоб приховати повідомлення про помилку в певних випадках. ## Дивіться також  - [Документація Gleam](https://gleam.run/) - [Вступ до програмування на Gleam](https://gleam.run/getting-started/introduction) - [Використання вбудованих функцій в Gleam](https://gleam.run/getting-started/built-in-functions)