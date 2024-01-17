---
title:                "Завантаження веб-сторінки"
html_title:           "Gleam: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Що і чому?
Завантаження веб-сторінок - це процес отримання вмісту веб-сторінки з Інтернету. Програмісти використовують цю технологію для отримання даних з веб-сайту для подальшого аналізу та обробки.

## Як це зробити:
```Gleam
import gleam/http

let result = http.get("https://www.example.com")
```
В даному прикладі, ми імпортуємо модуль "http" і використовуємо функцію "get" для завантаження веб-сторінки "https://www.example.com". Отриманий результат буде збережений в змінній "result".

## Глибші води:
Завантаження веб-сторінок є важливою частиною веб-розробки та автоматизації процесів. Раніше, для цього використовувалися різні інструменти та мови програмування, але з появою Gleam, завантаження стало значно простішим і зручнішим. Також, є інші альтернативи, наприклад, фреймворк "Reqwest" для мови програмування Rust.

## Дивіться також:
- [Офіційна документація Gleam](https://gleam.run/)
- [Gleam на GitHub](https://github.com/gleam-lang/gleam)
- [Стаття про Gleam на xd.adrian](https://xd.adrian.id.au/gleam/docs)