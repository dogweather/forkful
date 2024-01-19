---
title:                "Розбір HTML"
html_title:           "Arduino: Розбір HTML"
simple_title:         "Розбір HTML"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/lua/parsing-html.md"
---

{{< edit_this_page >}}

## Що і навіщо?

HTML-парсинг - це процес видобування даних з HTML-коду. Програмісти роблять це, щоб отримати корисну інформацію з веб-сторінок, які хочуть аналізувати.

## Як це робити:

Lua використовує бібліотеку "lua-htmlparser" для парсингу HTML. Нижче наведений приклад:

```Lua
local hp = require("htmlparser")
local html = "<title>Hello World</title>"
local root = hp.parse(html)
print(root("title"))  -- Виведе: Hello World
```

У цьому коді ми знаходимо тег `<title>` у нашому HTML і виводимо його вміст.

## Поглиблений розгляд

Парсинг HTML був широко використаний ще з ранніх днів інтернету, коли з'явились перші пошукові системы. Тепер він широко використовується у веб-скрапінгу, SEO, автоматизації браузера та іншому.

Є багато альтернатив Lua для парсингу HTML, таких як BeautifulSoup у Python або jsoup у Java. Вони мають схожі функції, але ваш вибір залежить від мови програмування, яку ви використовуєте, та ваших особистих вподобань.

Використовуючи `lua-htmlparser`, Lua проходить через HTML-код, визначає різні елементи та структурує їх у внутрішню модель даних, доступну для зручного пошуку і видобування інформації.

## Дивіться також

[Офіційна документація lua-htmlparser](https://github.com/msva/lua-htmlparser)

[Більше про HTML-парсинг](https://en.wikipedia.org/wiki/Web_scraping)