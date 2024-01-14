---
title:                "Elixir: Друк відладкового виводу"
simple_title:         "Друк відладкового виводу"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Зачем

Цей пост призначений для тих, хто хоче ознайомитися з використанням вихідного друку для відлагодження в Elixir програмуванні. Він покаже, як друкувати значення змінних та структур даних, що допоможе вам зрозуміти, як ваш код працює та виявити можливі проблеми.

## Як

Друк вихідних данних - це корисний інструмент для відлагодження в Elixir. Для того, щоб вивести значення змінних, достатньо використати функцію `IO.inspect/2` у вашому коді. Додавання цієї функції дозволить вам побачити значення змінної та структури даних у консолі.

Наприклад:

 ```Elixir
 name = "Oleksandr"
 IO.inspect(name)
 ```

Результат:

`"Oleksandr"`

Також ви можете друкувати кілька значень, вказавши їх у поданні як аргументи функції `IO.inspect/2`:

 ```Elixir
 name = "Oleksandr"
 last_name = "Ivanenko"
 IO.inspect(name, last_name)
 ```

Результат:

`%{"Oleksandr", "Ivanenko"}`

## Глибинна аналітика

Друк вихідного документу не тільки допомагає відлагоджувати проблеми, але й дозволяє вам отримати краще розуміння вашого коду. Ви можете встановити точки друку у різних місцях коду, щоб побачити, як змінюються значення певних змінних та вивчити, як код працює.

Також варто зазначити, що друк вихідних даних та використання функції `IO.inspect/2` можна комбінувати з іншими корисними інструментами, такими як `:debugger`, щоб отримати більш детальну інформацію.

## Дивіться також

- [Офіційна документація Elixir](https://elixir-lang.org/getting-started/debugging.html)
- [Відео "Debugging in Elixir"](https://www.youtube.com/watch?v=odZe9jK7l-w)
- [Стаття "Debugging in Elixir: Tips and Tricks"](https://hackernoon.com/debugging-in-elixir-tips-and-tricks-3b3c59441a7f)