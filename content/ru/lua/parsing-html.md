---
title:                "Разбор HTML"
date:                  2024-01-28T23:59:46.224170-07:00
model:                 gpt-4-0125-preview
simple_title:         "Разбор HTML"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/lua/parsing-html.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и зачем?

Разбор HTML означает преодоление лабиринта HTML-тегов для поиска необходимых данных. Программисты делают это для извлечения информации, автоматизации взаимодействия с вебом или миграции контента.

## Как:

Lua изначально не так хорошо подходит для работы с вебом, как Python или JavaScript, но с библиотеками `luasocket` и `luahtml` она может смело шагнуть в область разбора HTML. Давайте начнем с базового примера:

```Lua
local socket = require("socket.http")
local html = require("luahtml")

-- Получение HTML с URL
local body, code = socket.request("http://example.com")

if code ~= 200 then
    print("Не удалось загрузить страницу")
    return
end

-- Разбор HTML
local parsed_html = html.parse(body)

-- Извлечение данных из конкретного элемента, скажем, абзаца
for _, p in ipairs(parsed_html:select("p")) do
    print(p:getcontent())
end
```

Это выведет содержимое всех тегов абзаца (`<p>`) с полученной веб-страницы.

## Глубокое погружение

Разбор HTML в Lua - это не ситуация "один инструмент для всего". Вам придется сочетать различные библиотеки, в отличие от языков, созданных с учетом разбора веба. Исторически Lua была помощником для быстрой встроенной скриптовой разработки в приложениях, а не для веб-скрапинга.

Альтернативы? Помимо `luahtml`, существуют также `luascrape` и `luaxpath` для различных потребностей в разборе. Нет объективно "лучшего" выбора - каждый имеет свои особенности, с которыми вам придется разбираться.

Погружаясь в реализацию, библиотеки Lua, как правило, используют C API для повышения производительности. Производя разбор HTML, вы будете манипулировать узлами и элементами, каждый из которых - это возможность погнаться за каверзными деталями веб-структур.

## Смотрите также

- Документация LuaSocket: http://w3.impa.br/~diego/software/luasocket/http.html
- luahtml на GitHub для глубокого погружения в методы разбора: https://github.com/o-lim/luahtml
- Вики-сообщество пользователей Lua для жемчужин сообщества и устранения неполадок: http://lua-users.org/wiki/