---
title:                "Інтерполяція рядків"
date:                  2024-01-20T17:51:49.703474-07:00
model:                 gpt-4-1106-preview
simple_title:         "Інтерполяція рядків"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/lua/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Що це та навіщо?
Інтерполяція рядків у Lua – це спосіб вставляння змінних або виразів безпосередньо в рядок. Програмісти використовують її для динамічного складання тексту, що спрощує читання та мінімізує помилки.

## Як це зробити:
Так як чистий Lua (станом на останню версію) не підтримує вбудовану інтерполяцію рядків, ми можемо використовувати вбудовану функцію `string.format`:

```Lua
local name = "Іван"
local day = "понеділок"

-- Використовуючи string.format
local greeting = string.format("Привіт, %s! З нетерпінням чекаю зустрічі в %s.", name, day)
print(greeting)  -- Виведення: Привіт, Іван! З нетерпінням чекаю зустрічі в понеділок.
```

```Lua
local temperature = 23.5

-- Форматування з плаваючою точкою
local weather = string.format("Сьогодні температура повітря: %.1f градусів за Цельсієм.", temperature)
print(weather)  -- Виведення: Сьогодні температура повітря: 23.5 градусів за Цельсієм.
```

## Поглиблений розгляд
Споконвіку Lua не мав вбудованої підтримки інтерполяції рядків, на відміну від деяких інших мов програмування, як от Ruby або JavaScript. У Lua для роботи зі строками прийнято використовувати функцію `string.format`, яка схожа на свої аналоги в C і PHP, де використовуються спеціальні символи для форматування, наприклад `%s` для рядків чи `%d` для чисел.

Альтернативою для інтерполяції може бути конкатенація за допомогою оператора `..`, але це може бути менш зручно, коли потрібно вставити багато значень:

```Lua
local name = "Іван"
local age = 30

-- Конкатенація через ..
local introduction = "Мене звуть " .. name .. " і мені " .. age .. " років."
print(introduction)  -- Виведення: Мене звуть Іван і мені 30 років.
```

В якості додаткової опції можна писати свою функцію інтерполяції чи використовувати бібліотеки третіх сторін.

Поведінка `string.format` прямо залежить від переданих типів даних і специфікаторів форматування. Наприклад, використання `%f` для чисел з плаваючою точкою дає можливість контролювати кількість знаків після коми.

## Додаткові ресурси
- [Документація Lua `string.format`](https://www.lua.org/manual/5.4/manual.html#pdf-string.format)
- [Робота з рядками в Lua](https://www.lua.org/pil/20.html)