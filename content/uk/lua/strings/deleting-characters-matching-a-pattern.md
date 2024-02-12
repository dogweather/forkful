---
title:                "Видалення символів за візерунком"
date:                  2024-01-20T17:42:40.168586-07:00
model:                 gpt-4-1106-preview
simple_title:         "Видалення символів за візерунком"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/lua/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Що і Навіщо?
Видалення символів, що відповідають певному шаблону, це суть фільтрація рядку для певних цілей, наприклад, чищення даних. Програмісти роблять це для поліпшення тексту, видалення непотрібного або легкої зміни формату.

## Як це робити:
```Lua
-- видаляємо всі цифри з рядка
local text = "Hello123, що тут у нас?"
local cleaned_text = text:gsub("%d", "")
print(cleaned_text)  -- Виводить: Hello, що тут у нас?

-- видаляємо кириличні букви
local text = "Привіт213, а це що за language?"
local no_cyrillic = text:gsub("[%z\1-\127\194-\244][\128-\191]*", "")
print(no_cyrillic) -- Виводить: 213,  language?
```

## Поглиблений Розгляд
Видалення символів за шаблоном у Lua використовує регулярні вирази (patterns), але з певними особливостями, адресованими саме Lua. Історично, регулярні вирази беруть свій початок з теорії формальних мов і були популяризованіни обчислювальними програмами як спосіб пошуку чи зміни тексту. Альтернативними методами можуть бути функції обробки рядків, якщо завдання простіше, або використання зовнішніх бібліотек для складніших шаблонів. У Lua, `:gsub(pattern, replacement)` — ключова функція для таких завдань, де `pattern` є шаблоном, що вказує символи для видалення, а `replacement` — те, на що ми міняємо видалені символи. Ця функція ефективно працює для текстових маніпуляцій.

## Дивись Також
- [Programming in Lua (Fourth edition)](https://www.lua.org/pil/contents.html) - книга для глибшого занурення у мову програмування Lua.
- [Lua 5.4 Reference Manual](https://www.lua.org/manual/5.4/) - офіційний референс Lua, розділ про рядки і шаблони.