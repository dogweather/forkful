---
title:                "Utskrift av feilsøkingsutdata"
html_title:           "Elixir: Utskrift av feilsøkingsutdata"
simple_title:         "Utskrift av feilsøkingsutdata"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvorfor skulle noen involvere seg i å skrive ut feilsøkningsinformasjon? Debug output kan være en nøkkel til å identifisere og løse problemer i koden din, spesielt når du står fast og ikke helt forstår hva som skjer.

## Hvordan gjøre det
```elixir
IO.puts "Dette er en debug melding"
```

Resultat:
```elixir
"Dette er en debug melding"
```

For å skrive ut en debug-melding i Elixir, kan du bruke funksjonen `IO.puts`, som tar en hvilken som helst datatype som argument og skriver den ut.

En annen metode er å bruke `IO.inspect`, som lar deg se på verdien av en variabel midt i koden. Dette er spesielt nyttig for å sjekke verdier inne i en loop eller en funksjon.

```elixir
x = 2
IO.inspect x
```

Resultat:
```elixir
2
```

## Dypdykk
Det å skrive ut debug-informasjon er et viktig verktøy for å feilsøke koden din. Du kan også bruke `IO.puts` eller `IO.inspect` for å sjekke om en bestemt del av koden din blir utført, eller for å se på verdiene av variabler i forskjellige deler av koden.

Det er også mulig å bruke `IO.inspect` inne i en funksjon for å sjekke at argumentene som blir sendt inn, er riktige. Dette kan være spesielt nyttig når du jobber med funksjoner som tar mange argumenter og komplekse datatype.

En viktig ting å huske på er å ikke la debug output bli sittende igjen i koden din når den er ferdig og klar til å produseres. Sørg for å fjerne alle debug meldinger før du publiserer koden din.

## Se også
- [Offisiell Elixir Dokumentasjon](https://hexdocs.pm/elixir/IO.html)
- [Debugging in Elixir](https://medium.com/@azmy/elixir-debugging-helpers-cf4d7e4244d1)
- [Elixir Debugging Best Practices](https://www.poeticoding.com/elixir-debugging-best-practices/)