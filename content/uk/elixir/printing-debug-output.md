---
title:                "Elixir: Друк відлагоджувального виводу"
programming_language: "Elixir"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Для чого
Відображення відладочного виводу може бути корисним для вирішення проблем з вашою програмою або для вивчення її працездатності.

## Як
```Elixir
IO.puts("Це відладочне повідомлення.")
```

Вище показаний код додає відладочне повідомлення до вашої програми та виводить його на екран. Ви також можете використовувати інші функції, такі як `IO.inspect/2`, для відображення значень змінних або структур даних.

## Глибоке дослідження
Затримки в програмі можуть бути важко виявити без відображення відладочного виводу. Також, використання відладочного виводу може допомогти вам зрозуміти, як працює програма та допомогти в полегшенні пошуку та усуненні помилок.

## Дивіться також
- [Elixir документація по відлагодженню](https://elixir-lang.org/getting-started/debugging.html)
- [Відлагодження в Elixir](https://medium.com/@meddygarnet/debugging-in-elixir-9efb6adae810)
- [Відлагодження в Elixir з ExUnit](https://culttt.com/2016/04/06/debugging-elixir-exunit/)