---
title:                "Пошук та заміна тексту"
html_title:           "Lua: Пошук та заміна тексту"
simple_title:         "Пошук та заміна тексту"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/lua/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Що і чому?

Заміна і пошук тексту - це важлива частина програмування. Програмісти використовують цю техніку для швидкого і простого виправлення помилок, заміни застарілих кодів або знаходження конкретних частин тексту в програмі. Це дозволяє ефективно працювати з великими об'ємами коду і підтримувати його оновлення.

## Як це зробити:

```Lua
-- заміна всіх входжень цифр на відповідні літери
local text = "Hello 123 world!"
local alphabet = { "a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z" }
for i=1, #alphabet do
  text = string.gsub(text, i, alphabet[i])
end
print(text) --> Hello abc world!
```

```Lua
-- знаходження і заміна слова "лихо" на "добро"
local text = "Лихо несподівано, але добро завжди поруч"
text = string.gsub(text, "лихо", "добро")
print(text) --> Добро несподівано, але добро завжди поруч
```

## Глибше в темі:

Історично пошук і заміна тексту були складними і часом споживали багато ресурсів. Існували різні підходи, такі як перебір дерева (знайомі всім школярам зі слова "maslo") або використання алгоритму Бойера-Мура. Однак з введенням мови програмування Lua, яка підтримує регулярні вирази, процес став значно простішим і швидшим.

Існують інші підходи до заміни і пошуку тексту, такі як використання бібліотеки lrexlib або розширення регулярних виразів для потужнішого функціоналу.

## Дивись також:

Для більш детального розгляду теми рекомендуємо ознайомитись з офіційною документацією Lua по роботі з регулярними виразами: https://www.lua.org/pil/20.2.html

Також, можна використати онлайн редактори для перевірки регулярних виразів, наприклад, Regex101: https://regex101.com/

Хорошою практикою є також використання коментарів у коді при написанні регулярних виразів, щоб полегшити розуміння їх праці.