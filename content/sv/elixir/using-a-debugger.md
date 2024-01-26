---
title:                "Att använda en debugger"
date:                  2024-01-26T03:48:49.784631-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att använda en debugger"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/using-a-debugger.md"
---

{{< edit_this_page >}}

## Vad och Varför?
Att använda en debugger i Elixir innebär att stega igenom din kod, inspektera variabler och spåra flöden för att krossa buggar. Programmerare gör det för att förstå det oväntade och försäkra sig om att deras applikationer beter sig som avsett.

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