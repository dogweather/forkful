---
title:                "Få den gjeldende datoen"
html_title:           "Haskell: Få den gjeldende datoen"
simple_title:         "Få den gjeldende datoen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor? 

Å få dagens dato handler om å få den nøyaktige tiden og datoen i det nåværende øyeblikket. Dette er nyttig for mange programmeringsoppgaver, som å spore brukeraktivitet, timestamping og planlegging av fremtidige hendelser.

## Hvordan:

La oss se på hvordan du kan få dagens dato i Elixir. Vi bruker DateTime-funksjonene tilgjengelig i Elixir’s innebyggede bibliotek.

```elixir
dagens_dato = DateTime.now("Europe/Oslo")
IO.puts(dagens_dato)
```

Når du kjører koden ovenfor, vil du få en output som inneholder dagens dato. Eksempelet ovenfor bruker tidszonen for Oslo, Norge.

## Dyp Dykk:

1. Historisk Kontekst: Elixir, et dynamisk, funksjonelt språk utviklet for bygging og vedlikehold av skalerbare og vedlikeholdbare applikasjoner, inneholder innebyggede DateTime-moduler. Disse har blitt brukt til å hente og representere tid og dato siden versjon 1.3, sluppet i 2016.
2. Alternativer: I tillegg til DateTime-funksjoner, kan du også hente den nåværende tiden og dato ved bruk av :os.system_time eller :calendar.local_time i Erlang.
3. Implementeringsdetaljer: DateTime.now funksjonen i Elixir bruker systemets klokke og tidssoneinformasjon til å generere gjeldende tidspunkt.

## Se Også:

For dypere veiledning og dokumentasjon på dato og tid i Elixir, sjekk ut følgende kilder:

1. Offisiell Elixir Dokumentasjon for DateTime: https://hexdocs.pm/elixir/DateTime.html
2. Bruk av Elixir's innebygde DateTime module: http://learningelixir.joekain.com/learn-elixir-datetime/
3. Diskusjoner rundt DateTime i Elixir forum: https://elixirforum.com/t/date-time-in-elixir/14946