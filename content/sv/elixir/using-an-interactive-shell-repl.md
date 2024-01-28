---
title:                "Använda en interaktiv skal (REPL)"
date:                  2024-01-26T04:13:09.969061-07:00
model:                 gpt-4-0125-preview
simple_title:         "Använda en interaktiv skal (REPL)"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Vad & Varför?
En interaktiv skal, eller REPL (Read-Eval-Print Loop), låter dig prova kodsnuttar i realtid. Elixir-programmerare använder REPL, kallad IEx (Interactive Elixir), för att experimentera, felsöka och lära sig språket.

## Hur man gör:
För att starta IEx, öppna din terminal och skriv `iex`. Här är ett smakprov:

```Elixir
iex> name = "Elixir Programmer"
"Elixir Programmer"
iex> String.length(name)
17
iex> Enum.map([1, 2, 3], fn num -> num * 3 slut)
[3, 6, 9]
```

Resultatet ska visa variabeltilldelning, funktionens resultat och en anonym funktion i arbete.

## Djupdykning
IEx-skalet har varit en del av Elixir sedan dess tidiga dagar. José Valim, skaparen av Elixir, hämtade inspiration från interaktiva skal från andra språk som Pythons `python` och Rubys `irb`. Även om IEx delar många funktioner med dessa, är det byggt för att hantera Elixirs samtidiga natur och är fullt integrerat med Erlang VM:s kapaciteter.

Alternativ till IEx i Erlang-ekosystemet inkluderar `erl`, Erlang-skalet. Men IEx erbjuder en mer Elixir-vänlig miljö, med funktioner som omfattande tabbkompilering, historik och hjälpare.

IEx REPL är mer än en lekplats; den kan sömlöst ansluta till ett körande system. Detta är avgörande för felsökning av levande applikationer. Den underliggande implementeringen förlitar sig på BEAM (Erlang VM), vilket säkerställer att funktioner som het kodväxling stöds direkt i skalet.

## Se också
Kolla in dessa för vidare läsning och resurser:

- [Elixirs IEx-dokumentation](https://hexdocs.pm/iex/IEx.html)
- [Interaktiv Elixir (IEx) - Elixirs skal](https://elixir-lang.org/getting-started/introduction.html#interactive-elixir)
- [Erlangs `erl`-dokumentation](http://erlang.org/doc/man/erl.html)
- [Att lära sig Elixirs interaktiva skal](https://elixirschool.com/en/lessons/basics/iex_helpers/)
