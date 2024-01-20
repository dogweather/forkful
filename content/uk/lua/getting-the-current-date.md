---
title:                "Отримання поточної дати"
date:                  2024-01-20T15:15:53.999510-07:00
html_title:           "Bash: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/lua/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Що і Чому?
Отримання поточної дати - це процес зчитування системного часу, щоб дізнатися дату "зараз". Програмісти роблять це для логування, часових міток, або функцій, залежних від дати.

## Як це зробити:
```Lua
os.date("*t")  -- повертає таблицю з поточною датою та часом
```
Sample output:
```Lua
{
  year = 2023, month = 4, day = 5,
  hour = 14, min = 24, sec = 48,
  isdst = false -- чи зараз діє літній час
}
```
Також можна отримати дату як рядок:
```Lua
os.date()  -- повертає поточну дату і час як рядок
```
Sample output:
```Lua
"Wed Apr  5 14:24:48 2023"
```

## Поглиблений Розбір:
Lua використовує бібліотеку C `time.h` для роботи з часом, що забезпечує функцію `os.date()`. Іншим варіантом є `os.time()`, яка повертає час як кількість секунд з моменту Unix Epoch (1 січня 1970). Можемо використовувати це для отримання різниці в часі або додавання певного періоду до поточного моменту. Таблиця, яку повертає `os.date("*t")`, дозволяє легко маніпулювати окремими компонентами дати і часу. Важливо знати, що Lua не має власної розширеної бібліотеки для роботи з датами, як от в Python чи Java, це може ускладнити деякі завдання.

## Дивіться Також:
- Lua 5.4 Reference Manual: ос.date: [www.lua.org/manual/5.4/manual.html#pdf-os.date](http://www.lua.org/manual/5.4/manual.html#pdf-os.date)
- LuaRocks - "date" модуль для розширених функцій з датами: [luarocks.org/modules/tieske/date](https://luarocks.org/modules/tieske/date)