---
title:                "Зробити першу літеру рядка великою"
aliases: - /uk/lua/capitalizing-a-string.md
date:                  2024-02-03T19:06:10.752290-07:00
model:                 gpt-4-0125-preview
simple_title:         "Зробити першу літеру рядка великою"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/lua/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і Чому?
Приведення рядка до великих літер включає в себе зміну першого символа кожного слова в реченні на велику літеру, при цьому решта літер залишаються малими. Ця техніка часто використовується для форматування тексту для більш професійного або читабельного виводу, такого як підготовка заголовків або введення користувачем для відображення.

## Як це зробити:
Lua не має вбудованої функції для приведення рядків до великих літер, але ви легко можете досягти цього за допомогою базових функцій маніпуляції з рядками. Ось проста функція для приведення першої літери одного слова до великої літери:

```lua
function capitalize(word)
    return word:sub(1,1):upper() .. word:sub(2):lower()
end

print(capitalize("hello"))  -- Вивід: Hello
```

Щоб привести кожне слово в реченні до великої літери, ви можете розділити речення на слова, зробити кожне з них з великої літери, а потім з'єднати їх знову:

```lua
function capitalizeSentence(sentence)
    local words = {}
    for word in sentence:gmatch("%S+") do
        table.insert(words, capitalize(word))
    end
    return table.concat(words, " ")
end

print(capitalizeSentence("hello world from lua"))  -- Вивід: Hello World From Lua
```

Якщо ви працюєте над проектом, де ключовим є продуктивність і вам потрібні додаткові можливості маніпуляції з рядками, розгляньте можливість використання сторонньої бібліотеки, як-от `Penlight`. Penlight розширює Lua більш універсальними функціями обробки рядків, серед іншого:

```lua
-- Припускаючи, що Penlight встановлено:
local pl = require("pl.stringx")
local text = "hello lua users"
text = pl.capitalized(text)
print(text)  -- Вивід: Hello lua users

-- Зауважте: функція capitalized в Penlight приводить до великої літери лише перше слово.
-- Для приведення кожного слова до великої літери вам все ще доведеться реалізувати власне рішення або дослідити інші бібліотеки.
```
