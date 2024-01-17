---
title:                "Lese kommandolinje-argumenter"
html_title:           "Elixir: Lese kommandolinje-argumenter"
simple_title:         "Lese kommandolinje-argumenter"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Lesing av kommandolinje-argumenter er en vanlig prosedyre for utviklere, da den tillater dem å lese input fra brukeren uten å avbryte eller stoppe programmet. Dette gjør det mulig å gi interaktivitet til applikasjoner som kjører i en kommandolinje-terminal.

## Hvordan:

For å lese kommandolinje-argumenter i Elixir, kan du bruke funksjonen `System.argv/0`. Denne funksjonen returnerer en liste med alle argumentene som er gitt til programmet ved kjøring. Her er et eksempel på hvordan du kan bruke denne funksjonen:

```Elixir
args = System.argv()
IO.puts("Du har gitt følgende argumenter: #{inspect args}")
```

Om du kjører dette programmet med kommandolinje-argumenter, for eksempel: `elixir les_argumeter.ex arg1 arg2`, vil resultatet bli:

```bash
Du har gitt følgende argumenter: ["arg1", "arg2"]
```

## Dypdykk:

Å lese kommandolinje-argumenter har vært en viktig del av programmering siden tidlige dager. På den tiden da datamaskiner var store, tunge og dyre, var det vanlig å kjøre programmer gjennom kommandolinjen. Dette gjorde det nødvendig å gi argumenter til programmene på denne måten.

I dag har vi mange moderne grensesnitt som gjør kommandolinjen mindre vanlig, men det er fortsatt viktig å kunne lese og behandle input på denne måten i programmering.

Et alternativ til å bruke `System.argv/0` er å bruke et bibliotek som kalles `OptionParser`, som gir mer avanserte funksjoner for å lese og behandle kommandolinje-argumenter.

## Se Også:

- [Dokumentasjon for System.argv/0](https://hexdocs.pm/elixir/System.html#argv/0)
- [Dokumentasjon for OptionParser](https://hexdocs.pm/elixir/OptionParser.html)