---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:44.106161-07:00
description: 'Hoe te: Met de ingebouwde `Date` module van Elixir kun je eenvoudig
  met de tijdlijn spelen.'
lastmod: '2024-03-13T22:44:50.475572-06:00'
model: gpt-4-0125-preview
summary: Met de ingebouwde `Date` module van Elixir kun je eenvoudig met de tijdlijn
  spelen.
title: Een datum in de toekomst of het verleden berekenen
weight: 26
---

## Hoe te:
Met de ingebouwde `Date` module van Elixir kun je eenvoudig met de tijdlijn spelen.

```elixir
# Toevoegen aan of aftrekken van een gegeven datum
date_vandaag = ~D[2023-04-15]
{jaar, maand, dag} = date_vandaag

# Bereken een datum 10 dagen in de toekomst
date_toekomst = Date.add(date_vandaag, 10)
IO.inspect(date_toekomst)  # => ~D[2023-04-25]

# Bereken een datum 30 dagen in het verleden
date_verleden = Date.add(date_vandaag, -30)
IO.inspect(date_verleden)  # => ~D[2023-03-16]
```

Merk op hoe `Date.add/2` eenvoudig het aantal dagen neemt dat je wilt reizen in het tijdcontinuüm.

## Diepgaande Duik
De mogelijkheid om datums in de toekomst of het verleden te berekenen is niet nieuw. Historische programmeertalen hadden ook hun methodes—denk aan COBOL of FORTRAN. Echter, Elixir brengt functionele flair en de onveranderlijkheid van gegevens op tafel, waardoor datum berekeningen direct en minder gevoelig voor fouten zijn.

Alternatieven? Je zou handmatig kunnen berekenen door seconden, minuten, enzovoort toe te voegen, maar waarom het wiel opnieuw uitvinden als Elixir een robuuste `Date` module biedt? Vooral gezien tijdgebaseerde berekeningen complex kunnen worden, rekening houdend met schrikkeljaren, tijdzones en de veranderingen in zomertijd.

Implementatie details draaien om het begrijpen van Elixir's `:calendar` module en de onderliggende Erlang implementaties. We staan op de schouders van tijdperken van evolutie in datum- en tijdfuncties, met Elixir's syntaxis suiker die het allemaal nog zoeter maakt.

## Zie Ook
- Officiële documentatie van Elixir's `Date` module: https://hexdocs.pm/elixir/Date.html
- "Datum, Tijd, en Tijdzones in Elixir": Een artikel dat de mogelijkheden van Elixir op het gebied van tijdbeheer uitdiept.
- Documentatie van Erlang's `:calendar` module: http://erlang.org/doc/apps/erts/calendar.html
