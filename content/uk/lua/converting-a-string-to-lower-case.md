---
title:                "Перетворення рядка в нижній регістр"
html_title:           "Lua: Перетворення рядка в нижній регістр"
simple_title:         "Перетворення рядка в нижній регістр"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/lua/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Що та для чого?
Перетворення рядка у нижній регістр є процесом зміни всіх літер у рядку на їхні еквіваленти у нижньому регістрі (менші букви). Програмісти часто використовують цей процес для порівняння рядків чи для забезпечення рівності символів незалежно від їхньої регістрованої форми.

## Як це зробити:
```Lua
local str = "Hello World!"
local lower_str = string.lower(str)

print(lower_str) -- виведе "hello world!"
```

## Глибоке погруження:
Перетворення рядка у нижній регістр було посилено впродовж історії програмування, щоб уникнути непотрібних проблем з порівнянням рядків. Замість методу `string.lower()`, існують інші альтернативні шляхи зробити це, такі як використання модуля регулярних виразів.

## Дивіться також:
- [Метод string.lower() в документації Lua](https://www.lua.org/manual/5.3/manual.html#pdf-string.lower)
- [Регулярні вирази у Lua](https://www.lua.org/manual/5.3/manual.html#6.4.1)