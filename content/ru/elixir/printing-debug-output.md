---
title:                "Вывод отладочной информации на печать"
date:                  2024-01-29T00:00:23.097704-07:00
model:                 gpt-4-0125-preview
simple_title:         "Вывод отладочной информации на печать"

category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/elixir/printing-debug-output.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Вывод отладочной информации в Elixir заключается в отображении промежуточных результатов или значений переменных в консоли. Программисты делают это, чтобы отследить ошибки или понять, что их код делает в определенный момент выполнения.

## Как это сделать:

```elixir
defmodule DebugExample do
  def show_debug_output do
    name = "Elixir"

    IO.inspect(name, label: "Debug")
    # дальнейшая обработка
  end
end

DebugExample.show_debug_output()
# Вывод:
# Debug: "Elixir"
```

Это показывает самый простой способ что-то напечатать в консоли с использованием `IO.inspect/2`. Опция label добавляет пользовательский префикс, делая вывод более заметным.

## Подробнее

Функция `IO.inspect/2` в Elixir аналогична `puts` в Ruby или `console.log` в JavaScript. Это отлично подходит для быстрой и грязной отладки, практика которой стара как само программирование.

Альтернативы в Elixir включают в себя использование модуля `Logger` для более систематического уровневого ведения журнала приложений. Это более настраиваемо и подходит для производственной среды.

Что касается деталей реализации, `IO.inspect/2` возвращает данную информацию, что упрощает её вставку в поток без влияния на функциональность. Исторически Elixir всегда акцентировал внимание на инструментах для разработчиков, и функции вроде `IO.inspect/2` воплощают это, делая отладку более интегрированным опытом.

## Смотрите также

- Модуль IO в Elixir: https://hexdocs.pm/elixir/IO.html
- Введение в отладку в Elixir: https://elixirschool.com/en/lessons/specifics/debugging
- Официальное руководство по Logger: https://hexdocs.pm/logger/Logger.html
