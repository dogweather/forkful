---
title:                "Аналіз HTML"
date:                  2024-02-03T19:12:53.690471-07:00
model:                 gpt-4-0125-preview
simple_title:         "Аналіз HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/lua/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?
Розбір HTML передбачає вилучення даних та інформації з HTML-документів, що є важливим для веб-скрепінгу, аналізу даних та автоматизації завдань. Програмісти виконують це, щоб збирати, аналізувати або маніпулювати веб-контентом програмним способом, що дозволяє автоматизувати те, що інакше було б ручним вилученням даних з вебсайтів.

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
