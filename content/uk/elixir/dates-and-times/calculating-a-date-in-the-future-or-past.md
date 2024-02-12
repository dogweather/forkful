---
title:                "Обчислення дати у майбутньому або минулому"
aliases:
- /uk/elixir/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:30:42.908184-07:00
model:                 gpt-4-1106-preview
simple_title:         "Обчислення дати у майбутньому або минулому"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Навіщо?)
Розрахунок дати у майбутньому чи минулому - це визначення конкретного дня відносно іншої дати. Програмісти роблять це для створення нагадувань, резервування ресурсів, і обробки термінів дій.

## How to: (Як це зробити:)
```elixir
# Додавання днів до поточної дати
Date.add(Date.utc_today(), 10)

# Вивід
~D[2023-04-23]

# Відняття днів від дати
Date.add(~D[2023-04-13], -3)

# Вивід
~D[2023-04-10]
```

## Deep Dive (Поглиблено)
В історичному контексті, розрахунок дат використовували для ведення календарів та астрономічних обчислень. В Elixir ви можете використовувати модуль `Date`, що надає функції для роботи з датами. Варіанти включають використання бібліотеки Timex для більшої гнучкості. При розрахунку дати Elixir використовує UTC за замовчуванням – це усуває плутанину з часовими поясами.

## See Also (Дивіться також)
- [Elixir Date Documentation](https://hexdocs.pm/elixir/Date.html)
- [Timex GitHub Repository](https://github.com/bitwalker/timex)
- [Working with Time Zones in Elixir](https://hexdocs.pm/elixir/Time.html)
