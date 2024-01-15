---
title:                "Lesing av kommandolinje-argumenter"
html_title:           "Elixir: Lesing av kommandolinje-argumenter"
simple_title:         "Lesing av kommandolinje-argumenter"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

I denne artikkelen vil vi utforske hvordan man kan lese kommandolinjeargumenter i Elixir og hvorfor det kan være nyttig for utviklere. Kommandolinjeargumenter er definert som informasjon som leveres til et program når det kjøres i et terminalvindu. Dette kan for eksempel være flagg, filnavn eller andre parametere som påvirker hvordan programmet kjører.

## Hvordan gjøre det

For å lese kommandolinjeargumenter i Elixir, kan man bruke funksjonen `System.argv()` som returnerer en liste med argumentene som ble gitt til programmet ved kjøring. La oss se på et eksempel:

```Elixir
# Lag en enkel liste med tall fra 1 til 10
numbers = 1..10 |> Enum.to_list

# Kjør dette programmet fra terminalen og gi en liste med tall som argumenter
$ elixir read_args.exs 3, 7, 9

# I din Elixir-fil kan du lese argumentene slik:
args = System.argv()

# Bruk Enum-modulet for å konvertere argumentene fra strenger til heltall
input = args |> Enum.map(&String.to_integer/1)

# Bruk input til å filtrere ut tallene som ikke finnes i listen
filtered_numbers = numbers |> Enum.reject(fn(num) -> !Enum.member?(input, num) end)

# Skriv ut resultatet til terminalen
IO.puts "De filtrerte tallene er: #{filtered_numbers}"
```

```bash
De filtrerte tallene er: [3, 7, 9]
```

## Dypdykk

Det finnes en annen måte å lese argumenter på i Elixir ved hjelp av `OptionParser`-modulen som gir mulighet for å definere forskjellige alternativer og flagg som kan brukes ved kjøring av programmet. Dette kan være nyttig for å gi brukeren av programmet mer fleksibilitet og kontroll over hvordan det kjører.

En annen ting å merke seg er at man kan lese både argumenter og miljøvariabler ved hjelp av `System.argv()` og `System.get_env()`-funksjonene.

## Se også

- [Elixir dokumentasjon: System-modulen](https://hexdocs.pm/elixir/System.html)
- [Elixir dokumentasjon: OptionParser-modulen](https://hexdocs.pm/elixir/OptionParser.html)
- [Elixir dokumentasjon: Enum-modulen](https://hexdocs.pm/elixir/Enum.html)