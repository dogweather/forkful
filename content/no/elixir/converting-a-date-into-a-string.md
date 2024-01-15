---
title:                "Konvertere en dato til en streng"
html_title:           "Elixir: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere en dato til en streng kan være veldig nyttig når du arbeider med datoer i Elixir. Det kan tillate deg å vise datoen på en forståelig måte for brukeren eller formatere datoen på en annen måte enn standardformatet.

## Hvordan gjøre det

For å konvertere en dato til en streng, kan du bruke Elixir-funksjonen `~D` i kombinasjon med `to_string`-funksjonen. Her er et eksempel:

```elixir
~D[2020-01-01] |> to_string
```

Dette vil gi følgende utgang:

```
"2020-01-01"
```

Du kan også bruke `DateTime`-modulen til å konvertere en dato til en streng på et spesifikt format. For eksempel:

```elixir
DateTime.utc_now() |> DateTime.to_string(~c[yyyy-MM-dd HH:mm:ss])
```

Dette vil gi følgende utgang:

```
"2020-12-22 09:30:15"
```

## Dypdykk

Det er viktig å merke seg at når du konverterer en dato til en streng, vil datoen bli formatert i henhold til standarddatoformateringsregler for systemet ditt. Hvis du ønsker å formatere datoen på en annen måte, må du bruke spesifikasjonen for `to_string`-funksjonen.

Du kan også bruke `~D` sammen med `to_string` til å konvertere datoobjekter til strenger. For eksempel:

```elixir
Date.distant_future() |> to_string
```

Dette vil gi følgende utgang:

```
"9999-12-31"
```

Det er viktig å merke seg at datoer i Elixir er immutables, noe som betyr at de ikke kan endres. Derfor vil konverteringen av en dato til en streng alltid resultere i en ny streng og påvirker ikke det opprinnelige datoobjektet.

## Se også

- [Elixir Date Modul](https://hexdocs.pm/elixir/Date.html)
- [Elixir DateTime Modul](https://hexdocs.pm/elixir/DateTime.html)
- [Elixir String Modul](https://hexdocs.pm/elixir/String.html)