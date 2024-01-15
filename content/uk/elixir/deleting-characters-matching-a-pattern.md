---
title:                "Видалення символів, що відповідають шаблону"
html_title:           "Elixir: Видалення символів, що відповідають шаблону"
simple_title:         "Видалення символів, що відповідають шаблону"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Чому

За чим саме може стояти видалення символів, що відповідають заданому шаблону? Наприклад, якщо ви хочете очистити поток введення від всіх чисел.

## Як це зробити

```Elixir
# Приклад видалення чисел зі стрічки
input = "abc123def456ghi789"
output = String.replace(input, ~r/[0-9]/, "")
IO.puts output
# Результат: abcdefgh
```

## Глибоке занурення

У випадках, коли потрібно детальніше розібратися з видаленням символів, які відповідають заданому шаблону, можна використовувати функцію `Regex.run/3`, що повертає список знайдених співпадінь зі стрічкою. Також, зверніть увагу на параметр `:global`, який дозволяє видаляти всі співпадіння, а не тільки перше.

## Дивіться також

- Офіційна документація щодо функції `String.replace/3`: http://elixir-lang.org/docs/stable/elixir/String.html#replace/3
- Стаття про використання регулярних виразів в Еліксирі: https://medium.com/@pablo1n7roberts/using-regular-expressions-in-elixir-ace69b3f4eb6.