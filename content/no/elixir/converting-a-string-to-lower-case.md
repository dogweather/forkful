---
title:                "Elixir: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere en streng til små bokstaver kan være nyttig for å sikre konsistens og nøyaktighet i koden din, spesielt når du jobber med data som bruker forskjellige bokstavstørrelser.

## Hvordan

```Elixir
iex> String.downcase("ELIXIR")
"elixir"
```

```Elixir
iex> String.downcase("HeLLo WorLD")
"hello world"
```

## Dykk dypere

Når du bruker funksjonen `String.downcase/1`, konverteres hver enkelt bokstav i strengen til sin tilsvarende små bokstav. Dette gjøres ved hjelp av Unicode-standardene, som håndterer spesielle bokstaver og tegn i tillegg til de vanlige alfabetbokstavene.

For å ta det et steg videre kan du også bruke funksjonen `String.downcase!/1` for å konvertere strengen til små bokstaver uten å opprette en ny kopi. Dette kan være nyttig for å spare på minne og forbedre ytelsen i koden din.

## Se også

- [Elixir String Modulen](https://hexdocs.pm/elixir/String.html)
- [Unicode Standardene](https://www.unicode.org/standard/standard.html)