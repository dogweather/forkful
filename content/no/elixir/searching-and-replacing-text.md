---
title:                "Søke og erstatte tekst"
html_title:           "Elixir: Søke og erstatte tekst"
simple_title:         "Søke og erstatte tekst"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å søke og bytte ut tekst er en vanlig oppgave for programmerere. Dette gjøres for å erstatte en bestemt tekst med en annen i en tekstfil eller kodebase. Det kan være nyttig når du ønsker å gjøre en stor endring i koden din, og det ville være for tidkrevende å gjøre det manuelt.

## Slik gjør du:
```Elixir
tekst = "Hei, verden!"
ny_tekst = String.replace(text, "verden", "alle sammen")
IO.puts(ny_tekst)
```
Dette eksempelet demonstrerer hvordan du kan bruke `String.replace` funksjonen i Elixir for å bytte ut en del av en tekststreng med en annen. Output vil være "Hei, alle sammen!"

## Dykk dypere:
Søk og bytt ut tekst var en vanlig oppgave i tidligere programmeringsspråk, men Elixir gjør det enklere ved å tilby funksjoner som `String.replace`. Alternativer til å bruke innebygde funksjoner kan være å bruke regulære uttrykk eller å bruke en tekstredigerer. I Elixir, kan du også bruke funksjoner som `String.replace_first` og `String.replace_last` for mer spesifikke behov. Implementeringen av disse funksjonene bruker faktisk regular expressions under panseret.

## Se også:
- [Elixir String Modul Dokumentasjon](https://hexdocs.pm/elixir/String.html)
- [Regex i Elixir](https://elixirschool.com/en/lessons/basics/pattern-matching/#regular-expressions)