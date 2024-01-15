---
title:                "Отримання поточної дати"
html_title:           "Gleam: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Чому

Багато програм вимагають від користувачів введення поточної дати. Це допомагає відстежувати події, створювати звіти та багато іншого. Оглянемо, як найпростіше отримати поточну дату за допомогою Gleam.

## Як

Для отримання поточної дати в Gleam, ми можемо використати стандартну бібліотеку `Time` та її функцію `now`. Наприклад:

```Gleam
import Time

let current_date = Time.now()
```

Тепер у змінній `current_date` ми зберігаємо поточну дату у форматі `Time.Time`.

Далі ми можемо отримати окремі елементи дати, такі як рік, місяць, день чи час, використовуючи вбудовані функції бібліотеки `Time`. Наприклад, щоб отримати поточний рік, ми можемо використати `Time.year(current_date)`.

```Gleam
import Time

let current_date = Time.now()
let current_year = Time.year(current_date)
```

Цей приклад дозволить нам отримати поточний рік та зберегти його у змінну `current_year`.

## Глибокий даiв

Більше інформації про функцію `now` та інші функції бібліотеки `Time` можна знайти у [документації Gleam](https://gleam.run/documentation/?v=0.8.0). Там ви знайдете детальне опис функцій та приклади використання. Також, більш детально розбираючись з цими функціями, ви зможете розширити можливості своєї програми.

## Дивись також

- [Документація Gleam](https://gleam.run/documentation/?v=0.8.0)
- [Репозиторій Gleam на GitHub](https://github.com/gleam-lang/gleam)