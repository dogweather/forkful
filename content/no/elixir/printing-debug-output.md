---
title:                "Utskrift av feilrettingsutdata"
html_title:           "Elixir: Utskrift av feilrettingsutdata"
simple_title:         "Utskrift av feilrettingsutdata"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
Hvis du er en programmerer, har du sannsynligvis hørt uttrykket "skrive ut debug-utgang". Dette betyr ganske enkelt å skrive ut informasjon om hva som skjer i koden din mens den kjører. Dette kan være nyttig for å finne feil og forstå hvordan koden oppfører seg.

## Slik gjør du det:
Du kan enkelt skrive ut debug-utgang i Elixir ved å bruke funksjonen `IO.inspect`. Denne funksjonen tar et argument og skriver ut informasjonen til terminalen. For eksempel:

```Elixir
IO.inspect("Hei Verden!")
```

Dette vil skrive ut strengen "Hei Verden!" til terminalen. Du kan også skrive ut variabelverdier ved å inkludere dem som argumenter til `IO.inspect`. For eksempel:

```Elixir
navn = "Johan"
alder = 30
IO.inspect("Navn: #{navn}, Alder: #{alder}")
```

Dette vil skrive ut følgende:

```
Navn: Johan, Alder: 30
```

## Dypdykk:
Historisk sett, har programmerere brukt utskrift av debug-utgang som en enkel måte å feilsøke kode på. Men med fremveksten av moderne debuggingsverktøy, er ikke dette lenger den eneste måten å finne feil på. Noen alternativer til å skrive ut debug-utgang inkluderer å bruke breakpoints i en debugger, eller å bruke loggfiler.

`IO.inspect`-funksjonen er implementert på et lavt nivå i Elixir, og den kan ha en innvirkning på ytelsen til koden din. Derfor bør du være forsiktig med å bruke den for mye i produksjonskode.

## Se også:
- Offisiell Elixir dokumentasjon: https://hexdocs.pm/elixir/IO.html#inspect/2
- Elixir Forum diskusjon om `IO.inspect`: https://elixirforum.com/t/io-inspect-empty-line-after-value/13027