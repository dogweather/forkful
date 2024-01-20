---
title:                "Beregning av en dato i fremtiden eller fortiden"
html_title:           "Elixir: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å beregne en dato i fremtiden eller fortiden handler om å legge til eller trekke fra en gitt mengde tid fra en bestemt dato. Programmerere gjør dette for å forutsi inntektsframdrift, planlegge skriftlig innhold, håndtere påminnelser og mer.

## Hvordan:

Her er en grunnleggende måte å beregne en fremtidig dato på i Elixir:

```elixir
defmodule FutureDate do
  def calculate(days) do
    DateTime.utc_now()
    |> DateTime.add(days * 60 * 60 * 24, :second)
  end
end
```

Eksempel resultat:

```elixir
iex> FutureDate.calculate(30)
~U[2022-03-05T12:34:56Z]
```

## Dypdykk

- **Historisk Kontekst**: Funksjonaliteten for dato- og tidsoperasjoner ble introdusert i Elixir i versjon 1.3, og har siden blitt utvidet med bedre funksjoner i løpet av de påfølgende versjonene.

- **Alternativer**: Elixir-tidsbiblioteket er godt forankret for grunnleggende operasjoner, men for mer komplekse dato- og tidshåndteringsoperasjoner, kan man vurdere å bruke eksterne biblioteker som Timex.

- **Implementeringsdetaljer**: Innstillingen `DateTime.add/2` legger til eller trekker fra en angitt mengde tid i sekunder.

## Se også:

For mer avanserte brukssaker, her er noen kilder som kan være nyttige:

- Elixirs offisielle dokumentasjon på `DateTime`: https://hexdocs.pm/elixir/DateTime.html

- Dokumentasjon for Timex: https://hexdocs.pm/timex/Timex.html

- "Elixir School" på dato- og tidsfunksjoner: https://elixirschool.com/en/lessons/basics/date-time/