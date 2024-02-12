---
title:                "Завантаження веб-сторінки"
aliases:
- /uk/lua/downloading-a-web-page.md
date:                  2024-01-20T17:44:25.449517-07:00
model:                 gpt-4-1106-preview
simple_title:         "Завантаження веб-сторінки"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/lua/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Що & Навіщо?
Завантаження веб-сторінки — це процес отримання її вмісту через Інтернет. Програмісти роблять це для аналізу інформації, автоматизації задач і моніторингу вмісту.

## Як це робити:
В Lua, для завантаження веб-сторінок часто використовується бібліотека `socket.http`. Ось приклад:

```Lua
local http = require("socket.http")

-- Завантажуємо вміст сторінки
local body, statusCode, headers = http.request("http://example.com")

-- Перевіряємо чи успішне завантаження
if statusCode == 200 then
    print("Завантажено:")
    print(body)
else
    print("Помилка при завантаженні сторінки: " .. statusCode)
end
```

Ви отримаєте вміст веб-сторінки або повідомлення про помилку з HTTP кодом стану.

## Поглиблений Розгляд:
Завантаження веб-сторінки не завжди було таким простим. Ще в дні dial-up, це було тривалішим і складнішим процесом. Сьогодні, окрім `socket.http`, існують більш потужні інструменти як `LuaSec` для HTTPS з'єднань та `Lua-cURL` для більш складних запитів. Важливо розуміти відмінності між GET і POST запитами, заголовки запиту, та як обробляти редиректи чи куки.

## Дивіться Також:
- LuaSocket документація: http://w3.impa.br/~diego/software/luasocket/http.html
- LuaSec GitHub репозиторій: https://github.com/brunoos/luasec
- Lua-cURL бібліотека: https://github.com/Lua-cURL/Lua-cURL
