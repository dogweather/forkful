---
title:                "Использование интерактивной оболочки (REPL)"
aliases:
- /ru/elixir/using-an-interactive-shell-repl.md
date:                  2024-01-29T00:03:18.928512-07:00
model:                 gpt-4-0125-preview
simple_title:         "Использование интерактивной оболочки (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/elixir/using-an-interactive-shell-repl.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Интерактивная оболочка или REPL (Read-Eval-Print Loop, Цикл Чтение-Вычисление-Вывод) позволяет вам пробовать кусочки кода в реальном времени. Программисты Elixir используют REPL, называемый IEx (Interactive Elixir), для экспериментов, отладки и обучения языку.

## Как это сделать:
Чтобы запустить IEx, откройте терминал и введите `iex`. Вот небольшой пример:

```Elixir
iex> name = "Elixir Programmer"
"Elixir Programmer"
iex> String.length(name)
17
iex> Enum.map([1, 2, 3], fn num -> num * 3 end)
[3, 6, 9]
```

Вывод должен показывать присваивание переменных, результаты функций и работу анонимной функции.

## Подробнее
Оболочка IEx является частью Elixir с самых ранних дней. Жозе Валим, создатель Elixir, черпал вдохновение из интерактивных оболочек других языков, таких как `python` Python и `irb` Ruby. Хотя IEx разделяет множество функций с ними, она создана для обработки конкурентной природы Elixir и полностью интегрирована с возможностями виртуальной машины Erlang.

Альтернативы IEx в экосистеме Erlang включают в себя `erl`, оболочку Erlang. Но IEx предоставляет более дружелюбную к Elixir среду, с функциями, такими как обширное автодополнение, история и помощники.

REPL IEx - это не просто площадка для игр; она может без проблем подключаться к работающей системе. Это критически важно для отладки живых приложений. Основа реализации опирается на BEAM (виртуальная машина Erlang), что обеспечивает поддержку таких функций, как горячая замена кода, прямо в оболочке.

## Смотрите также
Посмотрите эти ресурсы для дополнительного чтения и информации:

- [Документация IEx от Elixir](https://hexdocs.pm/iex/IEx.html)
- [Интерактивный Elixir (IEx) - Оболочка Elixir](https://elixir-lang.org/getting-started/introduction.html#interactive-elixir)
- [Документация `erl` от Erlang](http://erlang.org/doc/man/erl.html)
- [Изучаем интерактивную оболочку Elixir](https://elixirschool.com/en/lessons/basics/iex_helpers/)
