---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:49.355180-07:00
description: "Het gebruik van een debugger in Elixir omvat het stapsgewijs doorlopen\
  \ van je code, het inspecteren van variabelen en het volgen van stromen om bugs\
  \ te\u2026"
lastmod: 2024-02-19 22:05:09.557936
model: gpt-4-0125-preview
summary: "Het gebruik van een debugger in Elixir omvat het stapsgewijs doorlopen van\
  \ je code, het inspecteren van variabelen en het volgen van stromen om bugs te\u2026"
title: Een debugger gebruiken
---

{{< edit_this_page >}}

## Wat & Waarom?
Het gebruik van een debugger in Elixir omvat het stapsgewijs doorlopen van je code, het inspecteren van variabelen en het volgen van stromen om bugs te verpletteren. Programmeurs doen dit om het onverwachte te begrijpen en ervoor te zorgen dat hun applicaties zich gedragen zoals ontworpen.

## Hoe te:
Elixir wordt geleverd met een ingebouwde grafische debugger genaamd `:debugger`. Om het te gebruiken, moet je het starten en koppelen aan je lopend proces.

Zorg er eerst voor dat je `:debugger` hebt gestart binnen een `iex`-sessie:
```elixir
iex> :debugger.start()
{:ok, #PID<0.108.0>}
```

Interpreteer nu de code module die je wilt debuggen:
```elixir
iex> :int.ni(MyApp.MyModule)
{:module, MyApp.MyModule}
```

Je kunt een breakpoint instellen:
```elixir
iex> :int.break(MyApp.MyModule, regelnummer)
:ok
```

En dan, voer je functie uit om de breakpoint te bereiken en door je code te stappen:
```elixir
iex> MyApp.MyModule.my_function(arg1, arg2)
# Debugger zal de uitvoering pauzeren op de regel met de breakpoint
```

## Diepgaande Duik
Voordat Elixir's `:debugger` er was, voorzag Erlang de debugger die Elixir gebruikt; het is robuust en geweldig in het omgaan met gelijktijdige processen, een sterk punt van Erlang VM (BEAM). In tegenstelling tot sommige andere debuggers, staat `:debugger` geen wijziging van variabelen on the fly toe, vanwege de onveranderlijke aard van data in Elixir. Wat betreft alternatieven, heb je `IEx.pry` dat je toelaat om de uitvoering te pauzeren en op elk punt in je code in een REPL te springen, wat super handig kan zijn.

Hoewel `:debugger` goed is voor een grafische interface, geven sommigen de voorkeur aan het ingebouwde `:observer`-hulpmiddel dat ook procesinspectie en systeemmetrieken biedt, zij het niet specifiek gericht op het stapsgewijs doorlopen van code. De gemeenschap van Elixir draagt ook bij aan hulpmiddelen zoals `visualixir` en `rexbug`, waardoor het ecosysteem van debuggereedschappen wordt uitgebreid voorbij de standaardopties.

## Zie Ook
- Officiële Elixir Getting Started Guide over Debugging: https://elixir-lang.org/getting-started/debugging.html
- Erlang's `:debugger` Documentatie: http://erlang.org/doc/apps/debugger/debugger_chapter.html
- Elixir Forum Discussies over Debugging Technieken: https://elixirforum.com/c/elixir-questions/elixir-questions-questions-help/15
