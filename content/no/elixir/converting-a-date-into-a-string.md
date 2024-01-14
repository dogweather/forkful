---
title:                "Elixir: Konvertering av en dato til en streng"
simple_title:         "Konvertering av en dato til en streng"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere en dato til en streng er et vanlig behov i programmering, spesielt når du arbeider med brukerinput eller datatilkoblinger. Elixir gjør dette enkelt og effektivt ved å tilby innebygde funksjoner for å håndtere både datoer og strenger.

## Slik gjør du det

For å konvertere en dato til en streng i Elixir, kan du bruke funksjonen `to_string` og angi datoen som argument. La oss si at vi ønsker å konvertere dagens dato til en streng:

```Elixir
iex> Date.to_string(Date.utc_today())
"2021-01-27"
```

Som du kan se, returnerte `to_string` funksjonen en streng i standarddatoformatet "YYYY-MM-DD". Men hva om vi ønsker å endre formatet? Da kan vi bruke funksjonen `format` sammen med en spesiell formatstreng. For eksempel, hvis vi ønsker å få datoen i formatet "DD. Month YYYY":

```Elixir
iex> Date.format(Date.utc_today(), "DD. Month YYYY")
"27. Januar 2021"
```

Det er også mulig å konvertere en tidspunkt til en streng ved hjelp av funksjonen `to_string` og tilsvarende funksjoner`format`. La oss se på et eksempel på hvordan du kan konvertere en dato og tid til en streng i formatet "DD/MM/YYYY HH:MM":

```Elixir
iex> DateTime.to_string(DateTime.utc_now(), "DD/MM/YYYY HH:MM")
"27/01/2021 16:30"
```

## Dypdykk

Nå som vi har sett hvordan du konverterer datoer og tidspunkter til strenger i Elixir, la oss dykke litt dypere inn i `format` funksjonen. Denne funksjonen tar to argumenter - en dato eller tid og en formatstreng. Formatstrengen bestemmer hvordan datoen eller tidspunktet vil bli presentert som en streng. Du kan bruke forskjellige formatkoder for å få ønsket resultat, for eksempel`D` for å få dagnummeret og `MMM` for å få månedsnavnet.

Det er også verdt å merke seg at Elixir har innebygde oversettelser for måneds- og ukedagnavn, slik at de vil bli presentert på ditt eget språk. Hvis du ønsker å bruke engelsk, kan du bruke `e` som en del av formatstrengen.

## Se også

- [Elixir dokumentasjon for Date](https://hexdocs.pm/elixir/Date.html)
- [Elixir dokumentasjon for DateTime](https://hexdocs.pm/elixir/DateTime.html)
- [Formatkoder for datoformatering i Elixir](https://elixirschool.com/en/lessons/basics/dates/)