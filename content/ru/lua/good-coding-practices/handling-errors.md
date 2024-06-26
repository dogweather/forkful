---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:58:41.515511-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: Lua \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u0442 \u0434\
  \u0432\u0435 \u043E\u0441\u043D\u043E\u0432\u043D\u044B\u0435 \u0444\u0443\u043D\
  \u043A\u0446\u0438\u0438 \u0434\u043B\u044F \u043E\u0431\u0440\u0430\u0431\u043E\
  \u0442\u043A\u0438 \u043E\u0448\u0438\u0431\u043E\u043A: `pcall` \u0438 `xpcall`.\
  \ \u0412\u043E\u0442 \u043A\u0430\u043A \u0438\u0445 \u0438\u0441\u043F\u043E\u043B\
  \u044C\u0437\u043E\u0432\u0430\u0442\u044C."
lastmod: '2024-03-13T22:44:45.302308-06:00'
model: gpt-4-0125-preview
summary: "Lua \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u0442 \u0434\u0432\
  \u0435 \u043E\u0441\u043D\u043E\u0432\u043D\u044B\u0435 \u0444\u0443\u043D\u043A\
  \u0446\u0438\u0438 \u0434\u043B\u044F \u043E\u0431\u0440\u0430\u0431\u043E\u0442\
  \u043A\u0438 \u043E\u0448\u0438\u0431\u043E\u043A."
title: "\u041E\u0431\u0440\u0430\u0431\u043E\u0442\u043A\u0430 \u043E\u0448\u0438\u0431\
  \u043E\u043A"
weight: 16
---

## Как это сделать:
Lua использует две основные функции для обработки ошибок: `pcall` и `xpcall`. Вот как их использовать:

```lua
function might_fail()
    if math.random() > 0.5 then
        error("Ой! Что-то пошло не так.")
    else
        print("Все хорошо!")
    end
end

-- Использование pcall
local success, errorMessage = pcall(might_fail)

if success then
    print("Успех!")
else
    print("Обнаружена ошибка:", errorMessage)
end

-- Использование xpcall с обработчиком ошибок
function myErrorHandler(err)
    print("Обработчик ошибок говорит:", err)
end

local status = xpcall(might_fail, myErrorHandler)
print("Был ли вызов успешным?", status)
```

Пример вывода может быть таким:

```
Обнаружена ошибка: Ой! Что-то пошло не так.
Обработчик ошибок говорит: Ой! Что-то пошло не так.
Был ли вызов успешным? false
```
Или, если ошибок не произошло:
```
Все хорошо!
Успех!
Все хорошо!
Был ли вызов успешным? true
```

## Глубокое погружение
Обработка ошибок, или "обработка исключений", не всегда была частью программирования. Ранние программы часто вылетали. По мере развития программирования возникла потребность в стабильности. Подход Lua прост по сравнению с некоторыми языками. Здесь нет блоков `try/catch`, только `pcall` и `xpcall`. Первая функция защищает вызов функции, возвращая статус и любые ошибки. Вторая добавляет функцию обработки ошибок, полезную для пользовательской очистки или логирования.

Альтернативой в Lua является использование `assert`, которое может служить аналогичной цели, выбрасывая ошибку, если его условие ложно. Но оно не так гибко, как `pcall`, для сложных сценариев обработки ошибок.

Внутренне, `pcall` и `xpcall` работают, создавая "защищенное окружение" для выполнения функции. Если возникает ошибка, окружение перехватывает ее и может либо обработать немедленно, либо передать назад программе для обработки.

## Смотрите также
- Книга "Программирование на Lua" (третье издание), доступная на https://www.lua.org/pil/ для подробного изучения обработки ошибок (раздел 8.4).
- Официальное руководство по Lua 5.4: https://www.lua.org/manual/5.4/ - для самой актуальной информации о функциях обработки ошибок Lua.
- Вики пользователей Lua об обработке ошибок: http://lua-users.org/wiki/ErrorHandling – для общественных идей и паттернов.
