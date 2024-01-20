---
title:                "Аналіз дати з рядка"
html_title:           "C++: Аналіз дати з рядка"
simple_title:         "Аналіз дати з рядка"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/lua/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Що це таке і навіщо?
Розбір дати з рядка - це спосіб отримання значення дати з текстового рядка. Програмісти роблять це, щоб обробити інформацію з різних джерел або форматів.

## Як це зробити:
Lua має вбудовану функцію `os.time`, яка дає нам можливість розібрати дату з рядка. Наприклад:

```Lua
local date_str = "07/29/2021"
local pattern = "(%d+)/(%d+)/(%d+)"
local xday, xmonth, xyear = date_str:match(pattern)
local converted_time = os.time({year = xyear, month = xmonth, day = xday})
print(converted_time)
```

Цей код використовує регулярні вирази для розбиття рядка дати на день, місяць і рік. Потім він створює новий об'єкт Date використовуючи `os.time`.

## Поглиблене вивчення
Розбір дати з рядка був використаний з самого початку програмування і все ще є необхідністю. Lua, як і багато інших мов програмування, має вбудовані функції, які допомагають нам з цим завданням.

Існує багато альтернатив засобів розбиття дати з рядка: ви можете написати власну функцію, що використовує регулярні вирази або повністю вбудовані функції аналізу дати.

Метод `os.time`, який ми використовували вище, використовує системний час комп'ютера для створення нового об'єкту часу. `os.time` приймає таблицю і повертає час в секундах що сплили від 1 січня 1970 року.

## Дивіться також
- [Lua User’s Wiki: Date and Time](http://lua-users.org/wiki/DateAndTime)
- [Tutorialspoint: Lua Strings](https://www.tutorialspoint.com/lua/lua_strings.htm)
- [Lua 5.3 Reference Manual](https://www.lua.org/manual/5.3/)