---
title:                "Gleam: Обчислення дати у майбутньому або минулому"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

Чому: Існує багато ситуацій, коли нам потрібно обчислити дату в майбутньому або минулому, такі як створення розкладу подій або планування подорожі.

Як: На щастя, у Gleam є вбудована функція для обчислення дати - `Date.add`. Для обчислення дати в майбутньому, ми просто передаємо кількість днів, які потрібно додати, у вигляді аргументу. Наприклад:

```Gleam
let today = Date.now()
let future_date = Date.add(today, 14) // Додаємо 14 днів до сьогоднішньої дати
IO.debug(show(future_date)) // Виводимо результат
```

Виведе ` 2009-02-01T00:00:00Z `.

Щоб обчислити дату в минулому, ми можемо використати від'ємне значення для аргументу `Date.add`. Наприклад, якщо ми хочемо відомість, яка була 14 днів тому, ми можемо написати:

```Gleam
let today = Date.now()
let past_date = Date.add(today, -14) // Віднімаємо 14 днів від сьогоднішньої дати
IO.debug(show(past_date)) // Виводимо результат
```

Виведе ` 2008-01-18T00:00:00Z `.

Глибокий погляд: Функція `Date.add` насправді працює з додатковим типом даних, відомим як `Duration` (Тривалість). Він представляє собою проміжок часу в певній одиниці (наприклад, дні або години). Якщо вам потрібно обчислити дату за допомогою інших одиниць, ви можете використати функцію `Duration.to_duration` для перетворення їх в `Duration`. Наприклад, якщо ви хочете додати 2 години до поточної дати:

```Gleam
let today = Date.now()
let two_hours = Duration.to_duration(2, Hour) // Конвертуємо 2 години в Duration
let future_date = Date.add(today, two_hours) // Додаємо 2 години до сьогоднішньої дати
IO.debug(show(future_date)) // Виводимо результат
```

Виведе ` 2020-11-05T02:00:00Z `.

Дивіться також: 

- [Офіційна документація Gleam](https://gleam.run/standard-library/date.html)
- [Відеоурок по роботі з датами у Gleam](https://www.youtube.com/watch?v=7PqbBqSqrzE)
- [Туторіал по використанню функції `Date.add`](https://github.com/lpil/gleam/blob/master/docs/tutorial/guide.md#manipulating-dates)