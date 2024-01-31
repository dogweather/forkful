---
title:                "Обчислення дати у майбутньому або минулому"
date:                  2024-01-20T17:31:30.142944-07:00
model:                 gpt-4-1106-preview
simple_title:         "Обчислення дати у майбутньому або минулому"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/lua/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Що і чому?

Обчислення дати в майбутньому чи минулому — це визначення дат після чи до вказаного моменту. Програмісти це роблять для розрахунку строків, планування подій або перевірки термінів давності.

## Як це зробити:

```Lua
os.date("*t", os.time() + 7 * 24 * 60 * 60) -- додати 1 тиждень
os.date("*t", os.time() - 7 * 24 * 60 * 60) -- відняти 1 тиждень

-- Приклад:
local today = os.time()
local next_week = os.date("*t", today + 7 * 24 * 60 * 60)
print("Сьогоднішній день:", os.date("%d/%m/%Y", today))
print("День через тиждень:", os.date("%d/%m/%Y", os.time(next_week)))
```

Output:
```
Сьогоднішній день: 06/04/2023
День через тиждень: 13/04/2023
```

## Поглиблене занурення:

Луа не має засобів для роботи з датами настільки гнучких, як у деяких інших мовах. `'os.time()'` і `'os.date()'` — основні функції для маніпуляцій з датами. Важливо розуміти часові зони та перехід на літній час, якщо точність є критичною.

Окрім Lua, інші мови як Python мають бібліотеки типу `datetime`, які пропонують більш розширені функції для роботи з датами. У Lua можна вдасться до зовнішніх бібліотек, як `luadate`, для складніших задач.

## Дивіться також:

- Lua 5.4 Reference Manual: [https://www.lua.org/manual/5.4/manual.html#6.9](https://www.lua.org/manual/5.4/manual.html#6.9)
- GitHub `luadate` Library: [https://github.com/Tieske/date](https://github.com/Tieske/date)
