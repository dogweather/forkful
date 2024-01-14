---
title:                "Elixir: Konvertere en dato til en streng"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Konvertering av datoer til strenger er en vanlig oppgave i programmering, spesielt i Elixir. Det kan være nyttig for å vise datoer til brukere eller formatere data til lagring. 

## Hvordan

For å konvertere en dato til en streng i Elixir, kan du bruke funksjonen `DateTime.to_string/2`. Denne funksjonen tar inn en dato og en formatstreng som argumenter.

```Elixir
dato = DateTime.utc_now()
format = "{{D, MMMM}} {{YYYY}}"
streng = DateTime.to_string(dato, format)

IO.puts(streng)

# Utskrift:
# 15. oktober 2021
```

Du kan også spesifisere et lokalt tidssone ved å legge til en liste med alternativer som et tredje argument i funksjonen.

```Elixir
dato = DateTime.utc_now()
format = "{DD.MM.YYYY}"
alternativer = [time_zone: "Europe/Oslo"]
streng = DateTime.to_string(dato, format, alternativer)

IO.puts(streng)

# Utskrift:
# 15.10.2021
```

Det finnes også andre funksjoner som kan være nyttige for å konvertere datoer til strenger, som for eksempel `DateTime.to_iso8601/1`, `Date.to_string/2` og `Time.to_string/2`. Disse funksjonene har lignende syntaks og kan også ta inn en liste med alternativer for å spesifisere en tidssone.

## Dypdykk

Når man skal konvertere datoer til strenger, er det også viktig å være klar over forskjellene mellom de ulike datatypene. I Elixir er det tre forskjellige datatyper som representerer datoer og klokkeslett: `Date`, `Time` og `DateTime`. `Date` representerer kun en dato, `Time` representerer kun et klokkeslett og `DateTime` representerer både dato og klokkeslett. Dette er viktig å være oppmerksom på når du skal konvertere og behandle data i din kode.

Du kan også bruke funksjonen `DateTime.from_iso8601/1` for å konvertere en ISO 8601-streng til en `DateTime`-dato. Dette kan være nyttig hvis du for eksempel henter data fra en ekstern API som bruker ISO 8601-formatet.

## Se også

- [Elixir DateTime dokumentasjon](https://hexdocs.pm/elixir/DateTime.html)
- [ISO 8601 dato- og klokkeslettstandard](https://en.wikipedia.org/wiki/ISO_8601)