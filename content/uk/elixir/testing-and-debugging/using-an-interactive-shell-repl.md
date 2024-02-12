---
title:                "Використання інтерактивної оболонки (REPL)"
date:                  2024-01-26T04:13:29.475790-07:00
model:                 gpt-4-0125-preview
simple_title:         "Використання інтерактивної оболонки (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Що і чому?
Інтерактивна оболонка або REPL (Read-Eval-Print Loop, Цикл "Читати-Виконати-Вивести"), дозволяє випробовувати фрагменти коду в реальному часі. Програмісти Elixir використовують REPL, який називається IEx (Interactive Elixir), для експериментів, налагодження та вивчення мови.

## Як користуватись:
Щоб запустити IEx, відкрийте термінал і наберіть `iex`. Ось невеликий приклад:

```Elixir
iex> name = "Elixir Programmer"
"Elixir Programmer"
iex> String.length(name)
17
iex> Enum.map([1, 2, 3], fn num -> num * 3 end)
[3, 6, 9]
```

Вивід має показати призначення змінних, результати функцій і анонімну функцію в дії.

## Поглиблений огляд
Оболонка IEx є частиною Elixir з самого початку. Жозе Валім, творець Elixir, черпав натхнення в інтерактивних оболонках інших мов, наприклад, оболонки `python` у Python та `irb` у Ruby. Хоча IEx має багато спільного з ними, вона створена для обробки конкурентної природи Elixir та повністю інтегрована з можливостями Erlang VM.

Альтернативами IEx в екосистемі Erlang є `erl`, оболонка Erlang. Але IEx забезпечує більш дружнє до Elixir середовище з такими можливостями, як всеосяжне завершення натисканням Tab, історія та помічники.

IEx REPL - це не просто майданчик для ігор; вона може беззатратно підключатись до працюючої системи. Це критично важливо для налагодження живих додатків. Основна реалізація покладається на BEAM (Erlang VM), гарантуючи підтримку таких можливостей, як гаряча заміна коду, прямо у оболонці.

## Дивіться також
Ознайомтесь з цими матеріалами для додаткового читання та ресурсів:

- [Документація IEx Elixir](https://hexdocs.pm/iex/IEx.html)
- [Інтерактивний Elixir (IEx) - оболонка Elixir](https://elixir-lang.org/getting-started/introduction.html#interactive-elixir)
- [Документація `erl` Erlang](http://erlang.org/doc/man/erl.html)
- [Вивчення інтерактивної оболонки Elixir](https://elixirschool.com/en/lessons/basics/iex_helpers/)
