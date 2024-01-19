---
title:                "Порівняння двох дат"
html_title:           "Lua: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/lua/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Що і чому?
Порівняння двох дат - це одна з найпоширеніших задач у програмуванні. Коли ми хочемо зробити щось в залежності від дати, нам необхідно зрозуміти, чи одна дата відбулася до, після або в той же час, що інша дата.

## Як це зробити:
Найпростішим і найбільш поширеним способом порівнювати дві дати в Lua є використання вбудованих функцій ```os.time()``` і ```os.difftime()```. Давайте подивимося на приклад коду:

```Lua 
my_date1 = os.time({year=2021, month=1, day=1})
my_date2 = os.time({year=2021, month=2, day=1})

time_difference = os.difftime(my_date2, my_date1)

print(time_difference)
```

Результатом буде виведення числа, яке представляє кількість секунд між двома датами. Якщо це число більше 0, це означає, що перша дата відбулася після другої, якщо менше 0 - перша була до другої, і 0 - обидві дати співпадають.

## Глибокий пір:
Функція ```os.time()``` використовує [Unix-час](https://uk.wikipedia.org/wiki/Unix-час) (число секунд, які пройшли з 1 січня 1970 року) для представлення дати і часу. Це стандартне представлення часу в більшості мов програмування.

Існують інші способи порівнювати дві дати, такі як використання бібліотеки [Date](https://luarocks.org/modules/tieske/date) або розпарсювання дати за допомогою [Lua String Patterns](https://www.lua.org/pil/20.2.html). Однак, використання вбудованих функцій - найпростіший і найшвидший спосіб вирішити цю задачу.

Найчастіше порівнюються дати з точністю до дня, але з використанням вбудованих функцій можна також порівнювати час до годин і хвилин.

## Дивіться також:
- [Документація Lua з вбудованими функціями](https://www.lua.org/manual/5.4/manual.html#6.9)
- [Модуль Date для Lua](https://luarocks.org/modules/tieske/date)