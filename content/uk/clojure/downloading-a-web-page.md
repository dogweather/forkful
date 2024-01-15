---
title:                "Завантаження веб-сторінки"
html_title:           "Clojure: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Чому

Можливо, ви хочете отримати доступ до веб-сторінки для подальшого аналізу її вмісту або використання інформації на ній.

## Як це зробити

```Clojure
(use 'clj-http.client) ; імпортуємо необхідну бібліотеку

(client/get "http://www.example.com") ; отримуємо вміст сторінки за допомогою HTTP GET запиту

```

Програма вище використовує бібліотеку `clj-http` для виконання HTTP запиту на веб-сторінку http://www.example.com. В результаті, ми отримуємо вміст сторінки у вигляді HTML коду. Далі, ви можете обробити цей вміст для виконання різних дій, таких як парсинг або витягування певної інформації.

## Занурення

Для виконання HTTP запитів у Clojure також можна використовувати бібліотеку `http.async.client`, яка дозволяє виконувати запити асинхронно, тобто без очікування відповіді. Це може бути корисно, коли необхідно злегка прискорити виконання вашої програми. Також, ви можете використовувати додаткові параметри запиту, такі як заголовки або параметри URL.

## Дивись також

- Офіційна документація по бібліотеці `clj-http`: https://github.com/dakrone/clj-http
- Документація по бібліотеці `http.async.client`: https://github.com/dakrone/clj-http
- Приклади використання бібліотеки `clj-http`: https://practicalli.github.io/clojure-webapps/web-scraping/http_client_lib_dependencies.html