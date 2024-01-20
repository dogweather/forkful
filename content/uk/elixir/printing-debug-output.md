---
title:                "Друк відлагоджувального виводу"
html_title:           "Arduino: Друк відлагоджувального виводу"
simple_title:         "Друк відлагоджувального виводу"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Що та навіщо?

Друк діагностичної інформації - це метод виводу даних на екран або в лог-файли для виявлення помилок при програмуванні. Програмісти використовують це для відстеження стану програми під час виконання.

## Як це робиться:

Приклади коду та вихідних даних в Elixir. Використовуйте `IO.inspect/2` для виводу даних на консоль:

```Elixir 
list = [1, 2, 3]
IO.inspect(list, label: "My List")
```

Вихідні дані:

``` 
My List: [1, 2, 3]
```

## Поглиблений розбір:

1. Історичний контекст: Функція `IO.inspect/2` була впроваджена в Elixir `1.0.0` в 2014 році. Вона набула популярності, тому що значно полегшила процес дебагу.

2. Альтернативи: Хоча в Elixir в основному використовується `IO.inspect/2` для дебага, можна використовувати також засоби для роботи з логами, наприклад, Logger.

3. Деталі реалізації: `IO.inspect/2` використовує модуль `Inspect`, який дає можливість кастомізації викликів `IO.inspect`.

## Дивіться також:

1. [Офіційна документація по `IO.inspect/2`](https://hexdocs.pm/elixir/IO.html#inspect/2)
2. [Стаття про дебаг в Elixir](https://hackernoon.com/debugging-in-elixir-b32e1a8d8f73)
3. [Повна документація по Elixir](https://elixir-lang.org/docs.html)