---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:17.275013-07:00
description: "Het kapitaliseren van een string betekent het maken van de eerste letter\
  \ van een gegeven string in hoofdletters - als het een letter is. Programmeurs doen\u2026"
lastmod: '2024-03-13T22:44:50.445259-06:00'
model: gpt-4-0125-preview
summary: Het kapitaliseren van een string betekent het maken van de eerste letter
  van een gegeven string in hoofdletters - als het een letter is.
title: Een string met hoofdletters maken
weight: 2
---

## Hoe te:
```elixir
# Een string kapitaliseren in Elixir
string = "elixir programmeren"
gekapitaliseerde_string = String.capitalize(string)
IO.puts gekapitaliseerde_string

# De output zal zijn:
# Elixir programmeren
```

```elixir
# Alle woorden in een string in hoofdletters
string = "elixir programmeertaal"
gekapitaliseerde_woorden = String.split(string)
                    |> Enum.map(&String.capitalize/1)
                    |> Enum.join(" ")

IO.puts gekapitaliseerde_woorden

# De output zal zijn:
# Elixir Programmeertaal
```

## Diepgaande Duik
Terug in de vroege dagen van het programmeren maakten programmeertalen zich vaak niet druk om stringmanipulatie als onderdeel van de kern van de taal. Elixir daarentegen komt met een robuuste module van stringfuncties direct uit de doos, dankzij de wortels in de volwassen Erlang VM (BEAM). Strings kapitaliseren in Elixir is een fluitje van een cent met de `String` module.

Naast de eenvoudige `String.capitalize/1`, kun je scenario's tegenkomen die complexer gedrag vereisen. Stel je moet titels of namen kapitaliseren op een cultureel gevoelige manier. De `String` module van Elixir alleen zal niet voldoende zijn; je zult kijken naar bibliotheken zoals `Cldr` voor internationaliseringsondersteuning.

Intern houdt `String.capitalize/1` rekening met Unicode en multibyte karakters, niet alleen ASCII. Dit betekent dat het correct omgaat met een breed scala aan talen en alfabetten, in plaats van alleen Engelse tekst.

Als alternatief kun je je eigen kapitalisatiefunctie maken, maar in de meeste gevallen moeten de ingebouwde methodes voldoende zijn. Met aangepaste implementaties open je de deur naar subtiele bugs, vooral met internationale tekst. Waarom het wiel opnieuw uitvinden als je hoogwaardige tools klaar hebt om te gebruiken?

## Zie Ook
- De officiële `String` documentatie van Elixir: [https://hexdocs.pm/elixir/String.html](https://hexdocs.pm/elixir/String.html)
- ExCldr-bibliotheek voor internationaliseringsondersteuning: [https://hex.pm/packages/ex_cldr](https://hex.pm/packages/ex_cldr)
