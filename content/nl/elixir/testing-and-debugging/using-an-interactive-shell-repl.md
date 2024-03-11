---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:07.791953-07:00
description: "Een interactieve shell, of REPL (Read-Eval-Print Loop), stelt je in\
  \ staat om codefragmenten in real-time uit te proberen. Elixir-programmeurs gebruiken\
  \ de\u2026"
lastmod: '2024-03-11T00:14:24.276945-06:00'
model: gpt-4-0125-preview
summary: "Een interactieve shell, of REPL (Read-Eval-Print Loop), stelt je in staat\
  \ om codefragmenten in real-time uit te proberen. Elixir-programmeurs gebruiken\
  \ de\u2026"
title: Het gebruik van een interactieve shell (REPL)
---

{{< edit_this_page >}}

## Wat & Waarom?
Een interactieve shell, of REPL (Read-Eval-Print Loop), stelt je in staat om codefragmenten in real-time uit te proberen. Elixir-programmeurs gebruiken de REPL, genaamd IEx (Interactive Elixir), voor experimenteren, debuggen en het leren van de taal.

## Hoe:
Om IEx te starten, open je terminal en typ je `iex`. Hier is een voorproefje:

```Elixir
iex> name = "Elixir Programmeur"
"Elixir Programmeur"
iex> String.length(name)
17
iex> Enum.map([1, 2, 3], fn num -> num * 3 end)
[3, 6, 9]
```

De uitvoer moet variabeletoewijzing, functieresultaten en een anonieme functie in actie laten zien.

## Diepe Duik
De IEx-shell is al sinds de vroege dagen een deel van Elixir. José Valim, de maker van Elixir, haalde inspiratie uit de interactieve shells van andere talen zoals Python's `python` en Ruby's `irb`. Hoewel IEx veel functies met deze deelt, is het gebouwd om de gelijktijdige aard van Elixir aan te kunnen en is het volledig geïntegreerd met de capaciteiten van de Erlang VM.

Alternatieven voor IEx in het Erlang-ecosysteem omvatten `erl`, de Erlang-shell. Maar IEx biedt een meer Elixir-vriendelijke omgeving, met functies zoals uitgebreide tab-aanvulling, geschiedenis en helpers.

De IEx REPL is meer dan een speelplaats; het kan naadloos verbinding maken met een draaiend systeem. Dit is cruciaal voor het debuggen van live applicaties. De onderliggende implementatie vertrouwt op de BEAM (de Erlang VM), en zorgt ervoor dat functies zoals hot code swapping rechtstreeks in de shell worden ondersteund.

## Zie Ook
Bekijk deze voor verder lezen en bronnen:

- [Elixir's IEx-documentatie](https://hexdocs.pm/iex/IEx.html)
- [Interactief Elixir (IEx) - De Elixir Shell](https://elixir-lang.org/getting-started/introduction.html#interactive-elixir)
- [Erlang's `erl` documentatie](http://erlang.org/doc/man/erl.html)
- [Het leren van Elixir’s Interactieve Shell](https://elixirschool.com/en/lessons/basics/iex_helpers/)
