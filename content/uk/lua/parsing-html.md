---
title:                "Парсинг HTML"
date:                  2024-01-20T15:33:15.416897-07:00
html_title:           "Arduino: Парсинг HTML"
simple_title:         "Парсинг HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/lua/parsing-html.md"
---

{{< edit_this_page >}}

## Що це таке та навіщо?
HTML-парсинг — це процес аналізу HTML-коду для отримання конкретних даних. Програмісти роблять це для автоматизації веб-скрапінгу, обробки контенту або інтеграції сервісів.

## Як це робити:
В Lua для парсингу HTML використовують зовнішні бібліотеки. Приклад з `lua-html`:

```Lua
local html = require("lua-html")

-- Завантаження HTML документу
local doc = html.parse([[
<html>
    <head><title>Test</title></head>
    <body>
        <p>Hello, World!</p>
    </body>
</html>
]])

-- Знаходимо елементи <p>
local paragraphs = doc:select("p")

for _, p in ipairs(paragraphs) do
    print(p:getcontent())  -- Виведення: Hello, World!
end
```

## Пірнаємо глибше:
У 90-х, коли HTML тільки починав набирати популярності, потреба у парсингу стала відчутною. З часом з'явилися спеціалізовані бібліотеки і встаткування. Lua не має вбудованого HTML-парсера, на відміну від Python з його `BeautifulSoup`. В Lua потрібно користуватися зовнішніми модулями, наприклад, `lua-html` або `htmlparser`.

Щодо реалізації, більшість парсерів використовують DOM або SAX для навігації по документу. Ключовою є відмінність між парсингом, який залежить від синтаксису (підвищена швидкість і менша гнучкість), і тим, який залежить від структури (легше справляється з некоректними HTML).

## Додатково:
- [W3C HTML5 Specification](https://www.w3.org/TR/html5/) – технічні деталі та специфікації HTML5.
