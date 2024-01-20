---
title:                "Завантаження веб-сторінки"
html_title:           "Gleam: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/lua/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Що і для чого?

Скачування веб-сторінки - це процес збереження вмісту веб-сайту на машину користувача. Програмісти це роблять, щоб аналізувати контент, виконувати web scraping або створювати резервні копії веб-сторінок.

# Як це зробити:

```Lua
local http = require("socket.http")
url = "http://www.lua.org"
body, code = http.request(url)

if code == 200 then
    print(body)
else
    print("HTTP request failed")
end
```

Вищезазначений код поверне весь HTML-вміст веб-сайту lua.org, якщо запит пройде успішно. В іншому випадку він виведе повідомлення про неудачу.

# Докладно:

1. Історичний контекст: Lua була створена 1993 року і з тих пір активно використовується для віддаленого управління, автоматизації та створення швидких прототипів.
2. Альтернативи: Load Web Page можна виконати в інших мовах, таких як Python (з допомогою бібліотеки requests) або JavaScript (з допомогою node-fetch).
3. Деталі реалізації: `http.request (url)` виконує HTTP GET запит до вказаного URL. Якщо запит успішний, функція повертає вибраний вміст веб-сторінки та HTTP код статусу 200.

# Див. також:

1. Документація по Lua: [тут](http://www.lua.org/manual/5.4/)
2. Додаткові інформації про HTTP запити: [тут](https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol)
3. Пояснення про web scraping: [тут](https://en.wikipedia.org/wiki/Web_scraping)