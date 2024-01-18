---
title:                "Капіталізація рядка"
html_title:           "Lua: Капіталізація рядка"
simple_title:         "Капіталізація рядка"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/lua/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## See Also:
Lua, also known as the programming language of the future, is a lightweight, efficient, and powerful language used by developers worldwide. In this article, we will be discussing how to capitalize a string in Lua and why it is an essential tool for programmers.

## Що & Чому? 
Капіталізація рядка це процес перетворення першої літери рядка з малої на велику. Програмісти роблять це, щоб покращити зрозумілість або форматування тексту, що є важливим для ефективності програмного коду.

## Як це зробити:
```Lua
-- Використовуйте функцію string.upper
local str = "привіт"
print(string.upper(str)) -- Виведе "ПРИВІТ"
```
```Lua
-- Використовуйте функцію string.upper и string.sub
local str = "привіт"
print(string.upper(string.sub(str, 1, 1))..string.sub(str, 2)) -- Виведе "Привіт"
```

## Deep Dive:
Капіталізація рядків виникла з необхідністю покращити читабельність тексту в старих комп'ютерних терміналах, які не мали можливості відображати різні регістри літер. Можна використовувати інші функції, такі як string.lower для перетворення рядка в малі літери або string.gsub для заміни символів у рядку. Також, існують різні бібліотеки та модулі для продвинутої обробки рядків у Lua.

## See Also:
https://www.lua.org - офіційний сайт Lua.
https://www.lua.org/manual/5.4/ - документація з функцій та модулів мови Lua.
https://lua-users.org/wiki/StringLibraryTutorial - корисний посібник по роботі з рядками у Lua.