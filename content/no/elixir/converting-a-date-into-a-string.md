---
title:    "Elixir: Konvertere en dato til en streng"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å konvertere en dato til en streng er en viktig del av programmering, spesielt i Elixir. Dette lar oss presentere datoer på en lesbar måte for brukere eller andre systemer. I tillegg kan det være nødvendig å konvertere datoer til strenger for å lagre dem i databaser eller sende dem over nettverket.

## Hvordan

I Elixir er det flere måter å konvertere en dato til en streng på, avhengig av dine behov og preferanser. La oss se på noen eksempler:

```elixir
# Konverter en dato til en streng med formatet "YYYY-MM-DD"
~D[2021-04-15] |> Date.to_string() # => "2021-04-15"

# Konverter en dato til en kort streng med månedsnavn
~D[2021-04-15] |> Date.to_string(:short) # => "15. apr"

# Konverter en dato til en lang streng med ukedag og månedsnavn
~D[2021-04-15] |> Date.to_string(:long) # => "torsdag 15. april 2021"
```

Som du kan se, bruker vi funksjonen `Date.to_string/2` for å konvertere datoen til en streng. Første argument er datoen vi ønsker å konvertere, og den andre er formatet vi ønsker å bruke. For mer informasjon om de ulike formatene og mulighetene som finnes, kan du sjekke Elixir sin offisielle dokumentasjon.

## Dykk dypere

Under overflaten, bruker Elixir det innebygde `Calendar` biblioteket for å håndtere datoer. Dette biblioteket gir oss mange nyttige funksjoner for å håndtere datoer og tid. For å konvertere en dato til en streng, bruker `Calendar.ISO.format/2` funksjonen under capnds (calendar to printable string) modulen.

## Se også

- [Elixir offisiell dokumentasjon for Date module](https://hexdocs.pm/elixir/Date.html#to_string/2)
- [Elixir offisiell dokumentasjon for Calendar module](https://hexdocs.pm/elixir/Calendar.html)