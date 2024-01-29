---
title:                "Een string met hoofdletters maken"
date:                  2024-01-28T21:56:17.275013-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een string met hoofdletters maken"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/elixir/capitalizing-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het kapitaliseren van een string betekent het maken van de eerste letter van een gegeven string in hoofdletters - als het een letter is. Programmeurs doen dit voor consistentie in de opmaak, het verfijnen van de gebruikersinterface of om aan datastandaarden te voldoen.

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
- Elixir School voor het leren over strings en andere basiszaken: [https://elixirschool.com/nl/lessons/basics/strings/](https://elixirschool.com/nl/lessons/basics/strings/)
- ExCldr-bibliotheek voor internationaliseringsondersteuning: [https://hex.pm/packages/ex_cldr](https://hex.pm/packages/ex_cldr)
