---
title:                "Розбір html"
html_title:           "Clojure: Розбір html"
simple_title:         "Розбір html"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/parsing-html.md"
---

{{< edit_this_page >}}

Що і Навіщо?

Парсинг HTML - це процес, за допомогою якого програмісти отримують доступ до структурованої інформації з веб-сторінок. Це може бути корисно при створенні веб-скраперів, які автоматично збирають інформацію з інших сайтів, або при аналізі інформації про товари на онлайн-магазинах.

Як це зробити:

```Clojure
(require '[net.cgrand.enlive-html :as html])

(html/emit (html/html-snippet "<div>Привіт, світ!</div>"))
```

Виведе:

```Clojure
"<div>Привіт, світ!</div>"
```

Поглиблення:

Історичний контекст: Парсинг HTML з'явився в 1990-х роках, коли почали поширювати стандартизовану мову розмітки веб-сторінок - HTML. До цього, програмісти використовували різні методи для отримання інформації з веб-сторінок, але з появою HTML стало можливим стандартизувати цей процес.

Альтернативи: Крім Clojure, існує багато інших мов програмування, які можуть бути використані для парсингу HTML. Наприклад, Python має бібліотеку BeautifulSoup, а JavaScript - Cheerio.

Деталі реалізації: Clojure використовує бібліотеку Enlive для парсингу HTML, яка дозволяє використовувати CSS-селектори для отримання елементів з веб-сторінки.

Дивіться також:

[Документація Enlive](https://github.com/cgrand/enlive)

[Туторіал з використання Enlive](https://purelyfunctional.tv/lesson/enlive-scraping-web-pages/)