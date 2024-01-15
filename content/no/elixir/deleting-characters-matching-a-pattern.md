---
title:                "Slette tegn som matcher et mønster."
html_title:           "Elixir: Slette tegn som matcher et mønster."
simple_title:         "Slette tegn som matcher et mønster."
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Bruken av å slette tegn som matcher et mønster er en vanlig oppgave innenfor webutvikling og tekstbehandling. Det kan være nyttig å lære denne ferdigheten for å effektivisere arbeidsflyten og gjøre det enklere å håndtere store mengder tekst.

## Hvordan gjøre det

Å slette tegn som matcher et mønster i Elixir er enkelt og kan gjøres ved hjelp av innebygde funksjoner. Først må du importere "Regex" biblioteket ved å skrive følgende kode:

```Elixir
import Regex
```

Deretter kan du bruke funksjonen "replace" som tar inn tre argumenter: en streng, et mønster og en erstatningsstreng. For eksempel, hvis du vil slette alle tall fra en streng, kan du gjøre følgende:

```Elixir
replace("Hello123World", ~r/\d+/,"")
```

Dette vil resultere i strengen "HelloWorld" uten tallene. Du kan også bruke et annet mønster for å slette andre typer tegn, for eksempel bokstaver eller spesialtegn.

## Dypdykk

I Elixir brukes "Regex" biblioteket til å arbeide med regulære uttrykk, som er et kraftig verktøy for å finne og manipulere tekst. Mønstre kan skrives på ulike måter, og det finnes mange ressurser online som kan hjelpe deg med å lære mer om det. Det er også mulig å bruke funksjonen "match?" for å sjekke om en streng samsvarer med et bestemt mønster. Dette kan være nyttig når du vil gjøre mer avanserte operasjoner enn bare å slette tegn.

## Se Også

- [Elixir Dokumentasjon: Regex](https://hexdocs.pm/elixir/master/Regex.html)
- [Regex Tutorial på Elixir Tutorial](https://elixir-tutorials.coding-gnome.com/advanced-strings-with-elixirs-regex-module/)
- [Elixir Cheat Sheet på Devhints](https://devhints.io/elixir)