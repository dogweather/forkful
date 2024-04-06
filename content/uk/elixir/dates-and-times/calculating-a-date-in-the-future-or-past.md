---
date: 2024-01-20 17:30:42.908184-07:00
description: "How to: (\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438:) ."
lastmod: '2024-04-05T21:53:48.984416-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u041E\u0431\u0447\u0438\u0441\u043B\u0435\u043D\u043D\u044F \u0434\u0430\u0442\
  \u0438 \u0443 \u043C\u0430\u0439\u0431\u0443\u0442\u043D\u044C\u043E\u043C\u0443\
  \ \u0430\u0431\u043E \u043C\u0438\u043D\u0443\u043B\u043E\u043C\u0443"
weight: 26
---

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
