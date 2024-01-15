---
title:                "Søking og utskifting av tekst"
html_title:           "Elixir: Søking og utskifting av tekst"
simple_title:         "Søking og utskifting av tekst"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Å søke og erstatte tekst er en vanlig oppgave når du jobber med programmering eller administrerer dokumenter. Det lar deg raskt og effektivt endre store mengder tekst basert på et gitt mønster.

## Hvordan

Søke og erstatte funksjonalitet er innebygd i Elixir ved hjelp av funksjonen `String.replace/3`. Denne funksjonen tar tre argumenter: den opprinnelige teksten, søkemønsteret og erstatningsstrengen.

```Elixir
iex> tekst = "Hei! Velkommen til Elixir!"
iex> String.replace(tekst, "Hei", "Hallo")
"Hallo! Velkommen til Elixir!"
```

Søkemønsteret kan også være en regulær uttrykk for mer fleksibel matching. For eksempel, hvis vi vil erstatte alle forekomster av små bokstaver med store bokstaver, kan vi bruke en regulær uttrykk med `~r` notasjon og regex funksjonen `String.upcase/1`.

```Elixir
iex> tekst = "Dette er en test"
iex> String.replace(tekst, ~r/[a-z]+/, &String.upcase/1)
"DETTE ER EN TEST"
```

## Deep Dive

String.replace-funksjonen i Elixir støtter også søk og erstatting i listen av atomer og lister av atomer. Du kan også bruke funksjonen `String.replace!/3` for å heve en `Regex.MatchError` hvis søkemønsteret ikke finnes i teksten.

## Se Også

- [Elixir String-modulen offisiell dokumentasjon](https://hexdocs.pm/elixir/String.html)
- [Regulære uttrykk i Elixir](https://elixir-lang.org/getting-started/string-patterns.html)