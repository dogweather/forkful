---
title:                "Виділення підрядків"
aliases:
- uk/elixir/extracting-substrings.md
date:                  2024-01-20T17:45:46.297840-07:00
model:                 gpt-4-1106-preview
simple_title:         "Виділення підрядків"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Що це таке & навіщо?
Витягування підрядків – це процес вибору певних частин тексту з рядка. Програмісти роблять це, щоб обробляти, валідувати або аналізувати специфічні дані всередині рядків.

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
