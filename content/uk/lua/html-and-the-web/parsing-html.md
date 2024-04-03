---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:53.690471-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : Lua \u043D\u0435 \u043C\u0430\u0454 \u0432\u0431\u0443\u0434\u043E\u0432\u0430\
  \u043D\u043E\u0457 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0438\
  \ \u0434\u043B\u044F \u0440\u043E\u0437\u0431\u043E\u0440\u0443 HTML, \u0430\u043B\
  \u0435 \u0432\u0438 \u043C\u043E\u0436\u0435\u0442\u0435 \u0432\u0438\u043A\u043E\
  \u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0432\u0430\u0442\u0438 \u0441\u0442\u043E\
  \u0440\u043E\u043D\u043D\u0456 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\
  \u0438, \u0442\u0430\u043A\u0456 \u044F\u043A `LuaHTML`, \u0430\u0431\u043E\u2026"
lastmod: '2024-03-13T22:44:49.495625-06:00'
model: gpt-4-0125-preview
summary: "Lua \u043D\u0435 \u043C\u0430\u0454 \u0432\u0431\u0443\u0434\u043E\u0432\
  \u0430\u043D\u043E\u0457 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0438\
  \ \u0434\u043B\u044F \u0440\u043E\u0437\u0431\u043E\u0440\u0443 HTML, \u0430\u043B\
  \u0435 \u0432\u0438 \u043C\u043E\u0436\u0435\u0442\u0435 \u0432\u0438\u043A\u043E\
  \u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0432\u0430\u0442\u0438 \u0441\u0442\u043E\
  \u0440\u043E\u043D\u043D\u0456 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\
  \u0438, \u0442\u0430\u043A\u0456 \u044F\u043A `LuaHTML`, \u0430\u0431\u043E \u0432\
  \u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0432\u0430\u0442\u0438\
  \ \u0437\u0432'\u044F\u0437\u043A\u0438 \u0437 `libxml2` \u0447\u0435\u0440\u0435\
  \u0437 `LuaXML`."
title: "\u0410\u043D\u0430\u043B\u0456\u0437 HTML"
weight: 43
---

## Як це зробити:
Lua не має вбудованої бібліотеки для розбору HTML, але ви можете використовувати сторонні бібліотеки, такі як `LuaHTML`, або використовувати зв'язки з `libxml2` через `LuaXML`. Популярним підходом є використання бібліотеки `lua-gumbo` для розбору HTML, яка надає просту можливість розбору, що відповідає стандарту HTML5.

### Встановлення lua-gumbo:
Спершу переконайтеся, що `lua-gumbo` встановлено. Зазвичай, його можна встановити за допомогою luarocks:

```sh
luarocks install lua-gumbo
```

### Базовий розбір за допомогою lua-gumbo:
Ось як ви можете розібрати простий фрагмент HTML та витягти з нього дані, використовуючи `lua-gumbo`:

```lua
local gumbo = require "gumbo"
local document = gumbo.parse[[<html><body><p>Привіт, світ!</p></body></html>]]

local p = document:getElementsByTagName("p")[1]
print(p.textContent)  -- Вивід: Привіт, світ!
```

### Розширений приклад - Витягування посилань:
Щоб витягти атрибути `href` з усіх якорних тегів (`<a>` елементів) у HTML-документі:

```lua
local gumbo = require "gumbo"
local document = gumbo.parse([[
<html>
<head><title>Зразкова сторінка</title></head>
<body>
  <a href="http://example.com/1">Посилання 1</a>
  <a href="http://example.com/2">Посилання 2</a>
  <a href="http://example.com/3">Посилання 3</a>
</body>
</html>
]])

for _, element in ipairs(document.links) do
    if element.getAttribute then  -- Переконайтеся, що це Елемент і що в нього є атрибути
        local href = element:getAttribute("href")
        if href then print(href) end
    end
end

-- Зразок виводу:
-- http://example.com/1
-- http://example.com/2
-- http://example.com/3
```

Цей фрагмент коду проходиться по всіх посиланнях у документі та виводить їх атрибути `href`. Здатність бібліотеки `lua-gumbo` розбирати та розуміти структуру HTML-документа спрощує процес витягування конкретних елементів на основі їх тегів або атрибутів.
