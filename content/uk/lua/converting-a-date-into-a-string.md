---
title:                "Перетворення дати в рядок"
html_title:           "Lua: Перетворення дати в рядок"
simple_title:         "Перетворення дати в рядок"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/lua/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Що & Чому?

Перетворення дати в рядок означає взяття дати у числовому форматі і перетворення його в рядок тексту. Це зроблено для того, щоб використовувати дату в різноманітних форматах в програмах і скриптах. Програмісти часто роблять це, щоб зробити дату більш зрозумілою для користувача або для збереження дати в базі даних.

## Як:

```Lua
-- Приклад 1: Використання функції os.date()
-- Перетворюємо поточну дату в рядок у форматі dd/mm/yyyy
local current_date = os.date("%d/%m/%Y") 
print(current_date) -- виводить "27/04/2021" (якщо сьогодні 27 квітня 2021 року)

-- Приклад 2: Використання користувацької функції
-- Перетворюємо дату "05/10/2020" в рядок у форматі "05 жовтня 2020"
function convert_date(input_date)
  local date_table = {} -- створюємо таблицю з датою
  date_table.day, date_table.month, date_table.year = string.match(input_date, "(%d+)/(%d+)/(%d+)") -- отримуємо день, місяць і рік з рядка
  local months = {"січня", "лютого", "березня", "квітня", "травня", "червня", "липня", "серпня", "вересня", "жовтня", "листопада", "грудня"} -- створюємо список місяців у словесній формі
  date_table.month = months[tonumber(date_table.month)] -- замінюємо номер місяця на його словесну назву
  local output_date = date_table.day .. " " .. date_table.month .. " " .. date_table.year -- перетворюємо дату знову в рядок
  return output_date -- повертаємо перетворену дату
end

local input_date = "05/10/2020"
local output_date = convert_date(input_date)
print(output_date) -- виводить "05 жовтня 2020"

-- Приклад 3: Використання сторонньої бібліотеки "date"

-- Підключаємо бібліотеку
local date = require("date") -- можливо потрібно буде здійснити установку бібліотеки перед використанням

-- Перетворюємо поточну дату в рядок у форматі "MM.DD.YYYY"
local current_date = date("%m.%d.%Y")
print(current_date) -- виводить "04.27.2021"

```

## Глибокий занурення:

### Історичний контекст
Перетворення дати в рядок було необхідною функцією з початку розвитку програмування, коли програми потребували зберігати і використовувати дати в різноманітних форматах.

### Альтернативи
Можна використовувати різні бібліотеки чи власні функції для перетворення дати в рядок у потрібному форматі. Наприклад, для мови Python є бібліотека datetime, яка має функцію strftime() для перетворення дати у текстовий формат.

### Деталі реалізації
Перетворення дати в рядок зазвичай здійснюється за допомогою форматування, яке визначає, яким чином дата повинна бути виведена (наприклад, день, місяць, рік в певному порядку із певним роздільником між ними). Використання правильного форматування дозволяє зберегти дату у вигляді, зрозумілому для людей, і одночасно зберегти її у числовому форматі для подальшого використання у програмах.

## Дивись також:

- [Функція os.date() в документації Lua](https://www.lua.org/pil/22.1.html)
- [Бібліотека datetime для Python](https://docs.python.org/3/library/datetime.html)