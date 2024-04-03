---
date: 2024-01-26 03:48:49.784631-07:00
description: "Elixir levereras med en inbyggd grafisk debugger som heter `:debugger`.\
  \ F\xF6r att anv\xE4nda den beh\xF6ver du starta den och koppla upp dig mot din\
  \ k\xF6rande\u2026"
lastmod: '2024-03-13T22:44:37.571969-06:00'
model: gpt-4-0125-preview
summary: Elixir levereras med en inbyggd grafisk debugger som heter `:debugger`.
title: "Att anv\xE4nda en debugger"
weight: 35
---

## Hur gör man:
Elixir levereras med en inbyggd grafisk debugger som heter `:debugger`. För att använda den behöver du starta den och koppla upp dig mot din körande process.

Först, se till att du har `:debugger` startad inom en `iex`-session:
```elixir
iex> :debugger.start()
{:ok, #PID<0.108.0>}
```

Nu, tolka den kodmodul du vill felsöka:
```elixir
iex> :int.ni(MyApp.MyModule)
{:module, MyApp.MyModule}
```

Du kan sätta en brytpunkt:
```elixir
iex> :int.break(MyApp.MyModule, line_number)
:ok
```

Och sedan köra din funktion för att träffa på brytpunkten och stega genom din kod:
```elixir
iex> MyApp.MyModule.my_function(arg1, arg2)
# Debuggern kommer pausa exekveringen vid raden med brytpunkten
```

## Fördjupning
Innan Elixirs `:debugger`, tillhandahöll Erlang debuggern som Elixir använder; den är robust och mycket bra på att hantera samtidiga processer, en stark sida hos Erlang VM (BEAM). Till skillnad från vissa andra debuggers tillåter `:debugger` inte ändring av variabler på flyget, på grund av det oföränderliga datanatur i Elixir. När det gäller alternativ har du `IEx.pry` som låter dig pausa exekveringen och hoppa in i en REPL vid valfri punkt i din kod, vilket kan vara superpraktiskt.

Medan `:debugger` är bra för ett grafiskt gränssnitt, kan vissa föredra det inbyggda `:observer`-verktyget som även erbjuder processinspektion och systemmetriker, även om det inte är specifikt inriktat på att stega genom kod. Elixirs community bidrar också med verktyg som `visualixir` och `rexbug`, vilket utvidgar ekosystemet av debuggverktyg bortom standardalternativen.

## Se också
- Officiella Elixir Getting Started Guide om Felsökning: https://elixir-lang.org/getting-started/debugging.html
- Erlangs `:debugger` Dokumentation: http://erlang.org/doc/apps/debugger/debugger_chapter.html
- Elixir Forum Diskussioner om Felsökningstekniker: https://elixirforum.com/c/elixir-questions/elixir-questions-questions-help/15
