---
title:                "Завантаження веб-сторінки"
html_title:           "Haskell: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Чому

Завантаження веб-сторінки може бути корисним для отримання необхідної інформації з Інтернету, наприклад, для створення власного веб-скрапера або для аналізу даних з різних сайтів.

## Як це зробити

Для початку необхідно встановити бібліотеку `http-conduit`, яка допоможе нам зробити запит до веб-сторінки. Далі, за допомогою функції `simpleHttp` ми можемо виконати запит та отримати відповідь у вигляді строки. Наприклад:

```Haskell
import Network.HTTP.Conduit
main = simpleHttp "https://www.example.com" >>= putStrLn . show
```

Результатом буде виведення HTML-коду цієї сторінки у консолі.

## Глибше копання

Бібліотека `http-conduit` дозволяє також виконувати більш складні запити, які включають передачу даних та роботу з кукісами. Для цього використовуються функції `withManager` та `withResponse`. Також існують різні додаткові опції, наприклад, можна вказати метод запиту (`GET`, `POST`, тощо), або встановити заголовки. Докладну інформацію можна знайти на офіційному сайті бібліотеки.

## Дивіться також

[Офіційна документація бібліотеки `http-conduit`](https://www.stackage.org/package/http-conduit)

[Туторіал по роботі з веб-скрапінгом в мові Haskell](https://www.fpcomplete.com/blog/2017/12/web-scraping-in-haskell)