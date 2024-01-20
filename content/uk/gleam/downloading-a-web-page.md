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

Завантаження веб-сторінки - це процес отримання вмісту веб-сторінки з сервера. Програмісти роблять це, щоб зробити аналіз даних, перевірити доступність веб-сайтів або розробити веб-скрепери.

## Як робити:
#### Встановлення
Спочатку потрібно встановити Gleam, використовуючи команду:
```shell
rebar3 new gleam_lib my_project
```
#### Кодування
Приклад коду для завантаження веб-сторінки в Gleam:
```Gleam
import gleam/http.{get}
import gleam/uri.{parse}
import gleam/option.{unwrap}

let my_request =
  parse("http://example.com")
  |> unwrap(_)
  |> get
```
Ви отримаєте результат, подібний на наступний:
```Gleam
Ok(
  Response(
    headers: [],
    status: 200,
    body: "Дані веб-сторінки"
  )
)
```
## Поглиблений погляд

1. **Історичний контекст**: Процес завантаження веб-сторінок виник з необхідності автоматизованого доступу до веб-контенту. Він користується неабиякою популярністю серед розробників-початківців і досвідчених програмістів.
2. **Альтернативи**: Існують інші інструменти для завантаження веб-сторінок, головним чином бібліотеки Python, такі як Requests та Beautiful Soup, а також Node.js інструменти, такі як Axios.
3. **Деталі реалізації**: Для завантаження веб-сторінки Gleam використовує HTTP-запити. Ви можете налаштувати параметри запитів, такі як заголовки або тіло запиту, за власним розсудом.

## Дивитись також

- Документація Gleam: https://gleam.run/docs/
- Руководство по HTTP у Gleam: https://gleam.run/book/tour/http-requests.html
- Більше про URI у Gleam: https://gleam.run/book/tour/uri.html