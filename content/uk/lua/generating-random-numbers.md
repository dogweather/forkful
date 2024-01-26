---
title:                "Генерація випадкових чисел"
date:                  2024-01-20T17:49:49.308530-07:00
model:                 gpt-4-1106-preview
simple_title:         "Генерація випадкових чисел"
programming_language: "Lua"
category:             "Lua"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/lua/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Згенерувати випадкове число — це дістати число, якого не можна передбачити. Програмісти це роблять, щоб додати непередбачуваність в ігри, симуляції, безпеку, та інші задачі.

## How to: (Як зробити:)
```Lua
math.randomseed(os.time()) -- ініціалізуємо генератор випадкових чисел

-- Генеруємо випадкове число від 1 до 10
local randNumber = math.random(1, 10)
print(randNumber) -- Виведе випадкове число, наприклад, 7

-- Генеруємо ще одне для прикладу
local anotherRandNumber = math.random(1, 10)
print(anotherRandNumber) -- Виведе інше випадкове число, наприклад, 3
```

## Deep Dive (Поглиблений Розгляд):
Генерація випадкових чисел у програмуванні історично важлива для криптографії, моделювання та розваг. У Lua, `math.random()` та `math.randomseed()` є основними функціями для цього. `math.randomseed()` встановлює "насіння" для генератора, забезпечуючи різні результати при кожному запуску, особливо коли використовується `os.time()`, що дає часову мітку. Без виклику `math.randomseed()`, `math.random()` може повертати однакові послідовності чисел при кожному запуску програми.

Lua використовує лінійний конгруентний метод для генерації псевдовипадкових чисел. Це простий, але не найкращий метод для високої криптографічної якості.

Альтернативою `math.random` може бути зовнішні бібліотеки, які пропонують більшу випадковість або використання апаратних генераторів випадкових чисел для критичних задач, де потрібна вища безпека.

## See Also (Дивіться Також):
- [Lua 5.4 Reference Manual: math.random](https://www.lua.org/manual/5.4/manual.html#pdf-math.random)
- [Lua 5.4 Reference Manual: math.randomseed](https://www.lua.org/manual/5.4/manual.html#pdf-math.randomseed)
- [Wikipedia on Pseudorandom Number Generators](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)
