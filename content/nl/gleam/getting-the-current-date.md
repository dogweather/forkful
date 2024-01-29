---
title:                "Het huidige datum ophalen"
date:                  2024-01-28T22:01:21.408530-07:00
model:                 gpt-4-0125-preview
simple_title:         "Het huidige datum ophalen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/gleam/getting-the-current-date.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

In programmeren betekent het verkrijgen van de huidige datum het ophalen van de echte, actuele datum. We doen dit voor het loggen van gebeurtenissen, tijdstempels voor transacties, of gewoon om dingen te plannen.

## Hoe te:

```Gleam
importeer gleam/kalender.{Datum, nu}
importeer gleam/io

pub fn hoofd() {
  laat vandaag: Datum = nu
  io.println(vandaag)
}
```

Voorbeelduitvoer:

```
Datum(jaar: 2023, maand: 4, dag: 14)
```

## Diepgaande Duik

Het concept van het ophalen van de huidige datum is zo oud als het computeren zelf. Het is geworteld in de behoefte om computeractiviteiten te relateren aan realtime gebeurtenissen. In Gleam maakt de `kalender` module het werken met datums een fluitje van een cent, met typen en functies zoals `Datum` en `nu`.

Voordat modules zoals deze bestonden, hadden ontwikkelaars vaak directe interactie met het besturingssysteem om datums op te halen. Dit kon lastig en foutgevoelig zijn.

Het `Datum` type in Gleam is een eenvoudige tuple struct, die je de componenten jaar, maand en dag geeft, respectievelijk. Achter de schermen zal `nu` over het algemeen de geschikte systeemniveau-API's aanroepen om de datum voor je te krijgen, waarbij de platformspecifieke verschillen worden geabstraheerd.

Alternatieven voor meer complexe datum- en tijdbewerkingen kunnen het gebruik van externe pakketten omvatten, aangezien de standaardbibliotheek van Gleam bewust minimaal wordt gehouden. Deze kunnen extra functionaliteiten bieden zoals tijdzones, opmaak en parsing.

## Zie Ook

- Gleam `kalender` module documentatie: https://hexdocs.pm/gleam_stdlib/gleam/calendar/
- Documentatie over Erlangs tijd functies, waarop Gleam kan leunen: https://erlang.org/doc/man/erlang.html#date-0
- Voor meer geavanceerde datum-tijd bibliotheken kunnen de aanbiedingen van de Elixir community, zoals 'Timex', beoordeeld worden voor potentiële interoperabiliteit: https://hex.pm/packages/timex
