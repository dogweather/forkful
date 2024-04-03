---
date: 2024-01-20 17:45:46.297840-07:00
description: "\u042F\u043A \u0446\u0435 \u0440\u043E\u0431\u0438\u0442\u0438: \u0423\
  \ Elixir \u0432\u0438\u0442\u044F\u0433\u043D\u0443\u0442\u0438 \u043F\u0456\u0434\
  \u0440\u044F\u0434\u043E\u043A \u043C\u043E\u0436\u043D\u0430 \u0434\u0435\u043A\
  \u0456\u043B\u044C\u043A\u043E\u043C\u0430 \u0441\u043F\u043E\u0441\u043E\u0431\u0430\
  \u043C\u0438. \u041D\u0438\u0436\u0447\u0435 \u043F\u043E\u043A\u0430\u0437\u0430\
  \u043D\u043E \u043F\u0430\u0440\u0443 \u043F\u0440\u0438\u043A\u043B\u0430\u0434\
  \u0456\u0432."
lastmod: '2024-03-13T22:44:48.704864-06:00'
model: gpt-4-1106-preview
summary: "\u0423 Elixir \u0432\u0438\u0442\u044F\u0433\u043D\u0443\u0442\u0438 \u043F\
  \u0456\u0434\u0440\u044F\u0434\u043E\u043A \u043C\u043E\u0436\u043D\u0430 \u0434\
  \u0435\u043A\u0456\u043B\u044C\u043A\u043E\u043C\u0430 \u0441\u043F\u043E\u0441\u043E\
  \u0431\u0430\u043C\u0438."
title: "\u0412\u0438\u0434\u0456\u043B\u0435\u043D\u043D\u044F \u043F\u0456\u0434\u0440\
  \u044F\u0434\u043A\u0456\u0432"
weight: 6
---

## Як це робити:
У Elixir витягнути підрядок можна декількома способами. Нижче показано пару прикладів:

```elixir
# Використання String.slice/3
original_string = "Привіт, світ!"
substring = String.slice(original_string, 0, 6)
IO.puts(substring) # Виводить "Привіт"

# Використання бінарних патернів
<<"Привіт", rest::binary>> = "Привіт, світ!"
IO.puts(rest) # Виводить ", світ!"
```

Перевірка розділів рядків через `String.split/2` та дістаємо підрядок:

```elixir
# Використання String.split/2 
parts = String.split("Привіт, світ!", ",")
IO.inspect(parts) # Виводить ["Привіт", " світ!"]
```

## Поглиблено:
Раніше стрічкові операції були досить коштовними для ресурсів. Сучасні мови, як Elixir, оптимізували обробку рядків. Elixir використовує UTF-8 і двійкові змінні для роботи з текстом, що дає простоту та швидкість. Альтернативи `String.slice/3` включають використання регулярних виразів з `Regex.run/3` чи функцій списків при роботі зі списками кодпоінтів Unicode.

## Подивіться також:
- [Elixir's Official String Docs](https://hexdocs.pm/elixir/String.html)
- [Elixir School's lessons on Strings](https://elixirschool.com/en/lessons/basics/strings/)
- [Programming Elixir ≥ 1.6 book](https://pragprog.com/titles/elixir16/programming-elixir-1-6/)
