---
date: 2024-01-26 03:48:51.884978-07:00
description: "\u042F\u043A \u043A\u043E\u0440\u0438\u0441\u0442\u0443\u0432\u0430\u0442\
  \u0438\u0441\u044F: Elixir \u043F\u043E\u0441\u0442\u0430\u0432\u043B\u044F\u0454\
  \u0442\u044C\u0441\u044F \u0437 \u0432\u0431\u0443\u0434\u043E\u0432\u0430\u043D\
  \u0438\u043C \u0433\u0440\u0430\u0444\u0456\u0447\u043D\u0438\u043C \u0434\u0435\
  \u0431\u0430\u0433\u0435\u0440\u043E\u043C \u043F\u0456\u0434 \u043D\u0430\u0437\
  \u0432\u043E\u044E `:debugger`. \u0429\u043E\u0431 \u0439\u043E\u0433\u043E \u0432\
  \u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0432\u0430\u0442\u0438\
  , \u0432\u0430\u043C \u043F\u043E\u0442\u0440\u0456\u0431\u043D\u043E \u0437\u0430\
  \u043F\u0443\u0441\u0442\u0438\u0442\u0438 \u0439\u043E\u0433\u043E \u0442\u0430\
  \u2026"
lastmod: '2024-03-13T22:44:48.731257-06:00'
model: gpt-4-0125-preview
summary: "Elixir \u043F\u043E\u0441\u0442\u0430\u0432\u043B\u044F\u0454\u0442\u044C\
  \u0441\u044F \u0437 \u0432\u0431\u0443\u0434\u043E\u0432\u0430\u043D\u0438\u043C\
  \ \u0433\u0440\u0430\u0444\u0456\u0447\u043D\u0438\u043C \u0434\u0435\u0431\u0430\
  \u0433\u0435\u0440\u043E\u043C \u043F\u0456\u0434 \u043D\u0430\u0437\u0432\u043E\
  \u044E `:debugger`."
title: "\u0412\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F \u0434\
  \u0435\u0431\u0430\u0433\u0435\u0440\u0430"
weight: 35
---

## Як користуватися:
Elixir поставляється з вбудованим графічним дебагером під назвою `:debugger`. Щоб його використовувати, вам потрібно запустити його та приєднатися до вашого запущеного процесу.

Спочатку, переконайтесь, що у вас запущено `:debugger` у сесії `iex`:
```elixir
iex> :debugger.start()
{:ok, #PID<0.108.0>}
```

Тепер інтерпретуйте модуль коду, який ви хочете дебагувати:
```elixir
iex> :int.ni(MyApp.MyModule)
{:module, MyApp.MyModule}
```

Ви можете встановити точку зупинки:
```elixir
iex> :int.break(MyApp.MyModule, line_number)
:ok
```

А потім запустіть вашу функцію, щоб активувати точку зупинки та крок за кроком пройтися по вашому коду:
```elixir
iex> MyApp.MyModule.my_function(arg1, arg2)
# Дебагер зупинить виконання на рядку з точкою зупинки
```

## Поглиблений аналіз
До використання `:debugger` у Elixir, Erlang надавав дебагер, який Elixir зараз використовує; він надійний і чудово справляється з одночасними процесами, що є сильною стороною Erlang VM (BEAM). На відміну від деяких інших дебагерів, `:debugger` не дозволяє змінювати змінні "на льоту" через незмінну природу даних в Elixir. Як альтернативу, ви можете використати `IEx.pry`, який дозволяє призупинити виконання і стрибнути в REPL в будь-якій точці вашого коду, що може бути дуже зручно.

Хоча `:debugger` і добре підходить для графічного інтерфейсу, деякі можуть віддати перевагу вбудованому інструменту `:observer`, який також пропонує інспектування процесів та метрики системи, хоча й не спеціалізується на прокроковому проходженні коду. Спільнота Elixir також пропонує інструменти на кшталт `visualixir` та `rexbug`, розширюючи екосистему інструментів для дебагу за межами стандартних.

## Дивіться також
- Офіційний початковий посібник Elixir по налагодженню: https://elixir-lang.org/getting-started/debugging.html
- Документація дебагера Erlang `:debugger`: http://erlang.org/doc/apps/debugger/debugger_chapter.html
- Дискусії на форумі Elixir про техніки налагодження: https://elixirforum.com/c/elixir-questions/elixir-questions-questions-help/15
