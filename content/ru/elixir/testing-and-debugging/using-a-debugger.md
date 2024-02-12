---
title:                "Использование отладчика"
date:                  2024-01-29T00:03:55.971750-07:00
model:                 gpt-4-0125-preview
simple_title:         "Использование отладчика"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/elixir/using-a-debugger.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
Использование отладчика в Elixir предполагает пошаговый просмотр кода, инспекцию переменных и отслеживание потоков для устранения ошибок. Программисты делают это, чтобы понять неожиданное и убедиться, что их приложения работают так, как задумано.

## Как использовать:
Elixir поставляется со встроенным графическим отладчиком, называемым `:debugger`. Чтобы использовать его, вам нужно его запустить и подключиться к вашему запущенному процессу.

Сначала убедитесь, что `:debugger` запущен в сессии `iex`:
```elixir
iex> :debugger.start()
{:ok, #PID<0.108.0>}
```

Теперь интерпретируйте модуль кода, который вы хотите отладить:
```elixir
iex> :int.ni(MyApp.MyModule)
{:module, MyApp.MyModule}
```

Вы можете установить точку останова:
```elixir
iex> :int.break(MyApp.MyModule, line_number)
:ok
```

А затем запустите вашу функцию, чтобы достигнуть точки останова и пройтись по вашему коду:
```elixir
iex> MyApp.MyModule.my_function(arg1, arg2)
# Отладчик приостановит выполнение на строке с точкой останова
```

## Подробнее
Перед `:debugger` Elixir, Erlang предоставил отладчик, который Elixir использует; он надежен и отлично справляется с параллельными процессами, что является сильной стороной Erlang VM (BEAM). В отличие от некоторых других отладчиков, `:debugger` не позволяет изменять переменные на лету из-за неизменяемой природы данных в Elixir. Что касается альтернатив, у вас есть `IEx.pry`, который позволяет приостановить выполнение и перейти в REPL в любой точке вашего кода, что может быть очень удобно.

Хотя `:debugger` хорош для графического интерфейса, некоторым может понравиться встроенный инструмент `:observer`, который также предлагает инспекцию процессов и системные метрики, хотя и не специально нацелен на пошаговый анализ кода. Сообщество Elixir также вносит свой вклад в инструменты, такие как `visualixir` и `rexbug`, расширяя экосистему инструментов отладки за пределы стандартных.

## Смотрите также
- Официальное руководство по началу работы с Elixir на тему отладки: https://elixir-lang.org/getting-started/debugging.html
- Документация Erlang `:debugger`: http://erlang.org/doc/apps/debugger/debugger_chapter.html
- Обсуждения техник отладки на форуме Elixir: https://elixirforum.com/c/elixir-questions/elixir-questions-questions-help/15