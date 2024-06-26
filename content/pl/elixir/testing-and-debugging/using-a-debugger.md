---
date: 2024-01-26 03:49:18.798157-07:00
description: "Jak to zrobi\u0107: Elixir jest dostarczany z wbudowanym graficznym\
  \ debuggerem o nazwie `:debugger`. Aby go u\u017Cy\u0107, musisz go uruchomi\u0107\
  \ i do\u0142\u0105czy\u0107 do\u2026"
lastmod: '2024-03-13T22:44:35.047574-06:00'
model: gpt-4-0125-preview
summary: Elixir jest dostarczany z wbudowanym graficznym debuggerem o nazwie `:debugger`.
title: Korzystanie z debugera
weight: 35
---

## Jak to zrobić:
Elixir jest dostarczany z wbudowanym graficznym debuggerem o nazwie `:debugger`. Aby go użyć, musisz go uruchomić i dołączyć do działającego procesu.

Najpierw upewnij się, że `:debugger` jest uruchomiony w sesji `iex`:
```elixir
iex> :debugger.start()
{:ok, #PID<0.108.0>}
```

Teraz zinterpretuj moduł kodu, który chcesz debugować:
```elixir
iex> :int.ni(MyApp.MyModule)
{:module, MyApp.MyModule}
```

Możesz ustawić punkt przerwania:
```elixir
iex> :int.break(MyApp.MyModule, line_number)
:ok
```

A następnie, uruchom swoją funkcję aby trafić na punkt przerwania i przejść przez swój kod:
```elixir
iex> MyApp.MyModule.my_function(arg1, arg2)
# Debugger zatrzyma wykonanie w linii z punktem przerwania
```

## Pogłębiona analiza
Przed `:debuggerem` Elixira, Erlang dostarczył debugger, którego Elixir używa; jest on solidny i świetnie radzi sobie z jednoczesnymi procesami, co jest mocną stroną Erlang VM (BEAM). W przeciwieństwie do niektórych innych debuggerów, `:debugger` nie pozwala na modyfikację zmiennych "w locie", ze względu na niezmienność danych w Elixirze. Jeśli chodzi o alternatywy, masz `IEx.pry`, który pozwala zatrzymać wykonanie i wskoczyć do REPL w dowolnym miejscu w kodzie, co może być bardzo przydatne.

Choć `:debugger` jest dobry do graficznego interfejsu, niektórzy mogą preferować wbudowane narzędzie `:observer`, które również oferuje inspekcję procesów i metryki systemowe, chociaż nie jest specjalnie przeznaczone do przestępowania przez kod. Społeczność Elixira również przyczynia się narzędziami takimi jak `visualixir` i `rexbug`, rozszerzając ekosystem narzędzi debugowania poza domyślne opcje.

## Zobacz także
- Oficjalny przewodnik po Elixirze dotyczący debugowania: https://elixir-lang.org/getting-started/debugging.html
- Dokumentacja `:debuggera` Erlanga: http://erlang.org/doc/apps/debugger/debugger_chapter.html
- Dyskusje na Forum Elixira na temat technik debugowania: https://elixirforum.com/c/elixir-questions/elixir-questions-questions-help/15
