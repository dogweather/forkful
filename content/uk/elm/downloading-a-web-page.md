---
title:                "Завантаження веб-сторінки"
html_title:           "Elm: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Чому
Не всі розуміють, чому потрібно завантажувати веб-сторінки, але це дуже корисна навичка для працівників IT і просто людей, які хочуть зберігати вміст сайтів для майбутнього використання. 

## Як використовувати
Завантаження веб-сторінок здійснюється у Elm за допомогою вбудованої бібліотеки `Http`. Спочатку потрібно імпортувати цю бібліотеку у ваш проєкт:
```Elm
import Http
```
Потім можна використати функцію `get` для отримання вмісту сторінки за заданим URL:
```Elm
url = "https://example.com"

Http.get url
    |> Http.send MyMsg
```
Результатом буде `MyMsg`, який містить усю інформацію про сторінку, включаючи заголовки, код статусу та вміст.

## Детальний огляд
У Elm є кілька функцій для завантаження веб-сторінок, включаючи `get`, `post` та `request` (яка дозволяє налаштувати різні параметри запиту, наприклад, заголовки або тіло запиту). Також можна використовувати функції `send` або `task` для обробки отриманої відповіді. Більш детальна інформація про ці функції доступна у [документації Elm](https://package.elm-lang.org/packages/elm/http/latest/Http). 

## Дивись також
- [Документація Elm по `Http`](https://package.elm-lang.org/packages/elm/http/latest/Http)
- [Приклади коду з використанням `Http`](https://elmprogramming.com/fetching-data-from-apis-in-elm-using-json-decoding.html)
- [Стаття про використання Elm для веб-розробки](https://medium.com/@robertkidder/why-i-like-elm-a-technical-review-of-elm-from-an-experienced-programmer-409a7d6dcdfa)