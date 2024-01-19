---
title:                "Перетворення рядка в нижній регістр"
html_title:           "Javascript: Перетворення рядка в нижній регістр"
simple_title:         "Перетворення рядка в нижній регістр"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/lua/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Що це таке і навіщо?

Перетворення рядка в нижній регістр - це процес виконання всіх букв рядка маленькими. Програмісти роблять це, щоб полегшити порівняння рядків і створити єдиний формат вводу/виводу.

## Як це зробити:

Lua має вбудовану функцію string.lower() для перетворення рядка в нижній регістр. Ось приклад використання:

```Lua
local text = "Hello, WORLD!"
lowercase_text = string.lower(text)
print(lowercase_text)
```

Цей код виведе: "hello, world!"

## Глибше занурення:

1. **Історичний контекст**: Володіти методами перетворення рядків є фундаментальною частиною будь-якої мови програмування. Lua, наче багато інших мов, включає в себе цю функцію.

2. **Альтернативи**: Якщо ви за якихось причин не хочете користуватися string.lower(), ви можете створити свою власну функцію. Очевидно, це буде менш ефективно і менш зручно.

3. **Деталі реалізації**: string.lower() в Lua працює так, що проходить по кожному символу в рядку і переводить його в нижній регістр, якщо це можливо. Якщо символ уже є в нижньому регістрі або не є буквою, він лишається таким, яким є.

## Дивіться також:

1. Офіційна документація Lua по рядкам: [http://www.lua.org/manual/5.3/manual.html#6.4](http://www.lua.org/manual/5.3/manual.html#6.4)

2. Глибше занурення в Lua рядки на Tutorialspoint: [https://www.tutorialspoint.com/lua/lua_strings.htm](https://www.tutorialspoint.com/lua/lua_strings.htm)

3. FAQ по Lua: [http://lua-users.org/wiki/LuaFaq](http://lua-users.org/wiki/LuaFaq)