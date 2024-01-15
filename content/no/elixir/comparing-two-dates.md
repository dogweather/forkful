---
title:                "Sammenligning av to datoer"
html_title:           "Elixir: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor

Vi har alle vært der - du har to forskjellige datoer og du må finne ut hvilken som er tidligere eller senere. I denne artikkelen skal vi se på hvordan du kan sammenligne to datoer på en enkel måte ved hjelp av Elixir programmeringsspråket.

## Slik gjør du det

Det første du trenger å gjøre er å definere to datoer som du vil sammenligne. Dette kan gjøres ved hjelp av Date modulen i Elixir.

```Elixir
start_date = Date.new(2021, 1, 1)
end_date = Date.new(2021, 1, 15)
```

For å sammenligne disse to datoene, kan vi bruke funksjonen `compare/2` som er tilgjengelig i Date modulen. Denne funksjonen tar inn to datoer og returnerer en liste med tre elementer: `-1`, `0` eller `1`, avhengig av om den første datoen er før, samme eller etter den andre datoen.

```Elixir
Date.compare(end_date, start_date)
# Output: [-1, 0, 1]
```

Disse tallene representerer følgende:

- `-1`: Den første datoen er tidligere enn den andre
- `0`: Begge datoene er like
- `1`: Den første datoen er senere enn den andre

Som du kan se, er det enkelt å sammenligne to datoer ved hjelp av Elixir. Du kan også bruke denne funksjonen til å sammenligne datoer og klokkeslett, da den også støtter tidskoder. 

## Dykk dypere

Hvis du ønsker å dykke dypere inn i hvordan Elixir sammenligner datoer, kan du undersøke koden i Date modulen. Du vil finne at den bruker en algoritme kalt "The Modified Julian Day Count" (MJD) for å sammenligne datoer. Denne algoritmen konverterer datoene til tallverdier og sammenligner dem deretter.

En annen interessant ting å merke seg er at Elixir også har en funksjon kalt `date/0` som returnerer dagens dato. Dette kan være nyttig for å sammenligne datoer og klokkeslett med nåværende tidspunkt.

## Se også

- Offisiell Elixir dokumentasjon for Date modulen: https://hexdocs.pm/elixir/Date.html
- Elixir School sin guide om datoer: https://elixirschool.com/no/lessons/basics/dates/