---
title:                "Omgjøring av dato til en streng"
html_title:           "Elixir: Omgjøring av dato til en streng"
simple_title:         "Omgjøring av dato til en streng"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Konvertering av datoer til strenger er en vanlig oppgave for mange programmerere når de jobber med ulike datatyper. Dette gjøres ofte for å kunne presentere datoen på en mer lesbar måte for brukere av applikasjonen.

## Hvordan:
```Elixir
iex> {:ok, dates} = Date.range(~D[2020-01-01], ~D[2020-01-10])
iex> Enum.map(dates, &to_string/1)
["2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04", "2020-01-05",
 "2020-01-06", "2020-01-07", "2020-01-08", "2020-01-09", "2020-01-10"]
```
Her ser vi et enkelt eksempel på hvordan vi kan konvertere en dato til en streng ved hjelp av Elixir. Vi starter ved å opprette et område med datoen 1. januar til 10. januar og deretter mapper vi datoene til strenger ved hjelp av funksjonen `to_string/1`.

## Dypdykk:
Konvertering av datoer til strenger er en nødvendig prosess i mange programmeringsspråk. Datoer er ofte lagret i programmet på en spesifikk måte, for eksempel som et heltall eller et tuple, men når vi ønsker å presentere dette for en bruker, er det ofte mer ønskelig å vise datoen som en streng, for eksempel "10. januar 2020". Dette gjøres ved hjelp av funksjoner som `to_string/1` eller `DateTime.to_iso8601/1` i Elixir.

Det finnes også alternative måter å konvertere datoer til strenger på, for eksempel ved å bruke moduler som `Timex` eller `Calendar` som tilbyr flere funksjoner for å formatere datoen på ulike måter. Det er også viktig å være oppmerksom på at ulike kulturer og regioner bruker ulike formater for datoer, og det kan være nyttig å tilpasse strengformatet basert på brukernes preferanser.

I Elixir er konvertering av datoer til strenger implementert gjennom funksjoner som er en del av standardbiblioteket. Dette betyr at de kan brukes uten å måtte installere eksterne pakker eller moduler.

## Se Også:
- [Elixir Date modul](https://hexdocs.pm/elixir/Date.html)
- [Elixir DateTime modul](https://hexdocs.pm/elixir/DateTime.html)
- [Elixir Timex modul](https://hexdocs.pm/timex/readme.html)
- [Elixir Calendar modul](https://hexdocs.pm/calendar/readme.html)