---
title:                "Beräkning av ett datum i framtiden eller förflutna"
html_title:           "Elixir: Beräkning av ett datum i framtiden eller förflutna"
simple_title:         "Beräkning av ett datum i framtiden eller förflutna"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Beräkna ett datum i framtiden eller förflutna är att fastställa ett specifikt datum före eller efter ett visst antal dagar. Programmerare gör detta för att hantera alla händelser relaterade till tidsstyrning som uppgiftsplanering, påminnelser etc.

## Så här gör du:
Använd modulen `DateTime` i Elixir för att arbeta med datum. Här är ett exempel:

```Elixir
# Nuvarande datum och tid
iex> nu = DateTime.utc_now()
# Datum för 7 dagar framåt
iex> framtid = DateTime.add(nu, 7 * 24 * 60 * 60, :second)
# Datum för 7 dagar bakåt
iex> forflutna = DateTime.add(nu, -7 * 24 * 60 * 60, :second)
```

Resultatet skulle vara datum och tid för 7 dagar framåt och bakåt från nuvarande datum och tid.

## Fördjupning
Historiskt har datumhantering varit en utmaning inom programmering på grund av inkonsekvensen i kalendersystemet och tidszoner. Elixir underlättar detta med `DateTime` modulen som hanterar dessa inkonsekvenser.

Alternativ till att använda `DateTime` inkluderar att använda andra datum och tids bibliotek som `Timex`.

När det gäller implementeringsdetaljer, använder `DateTime.add/3`-funktionen antalet sekunder i en minut, minuterna i en timme och timmarna på en dag för att beräkna det framtida eller förflutna datumet. Det är därför vi multiplicerar antalet dagar med 24 (timmar), 60 (minuter) och 60 (sekunder) för att få totalt antal sekunder.

## Se även
Elixir's officiella dokumentation på DateTime:
[DateTime — Elixir v1.12.3](https://hexdocs.pm/elixir/DateTime.html)

Guide på hur man jobbar med `DateTime` i Elixir:
[Working with dates and time in Elixir](https://inquisitivedeveloper.com/lwm-elixir-16/)

Elixir School's guide om `DateTime`, `Date` och `Time`:
[Elixir School](https://elixirschool.com/en/lessons/basics/date_time)