---
title:                "Elixir: Søking og Erstatning av Tekst"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

I Elixir er tekstbehandling et uunnværlig verktøy for enhver programmør. Det gjør det enkelt å finne og erstatte deler av tekst i koden din, noe som kan være svært nyttig når du jobber med store eller komplekse prosjekter. Så hvorfor bør du bruke tid på å lære hvordan du kan søke og erstatte tekst i Elixir? Fordi det kan spare deg for massevis av tid og arbeid når du skal gjøre endringer i koden din.

## Hvordan

Det første du må gjøre er å lagre teksten du ønsker å søke og erstatte i en variabel. La oss si at du ønsker å endre "hei" til "hallo" i en tekststreng. Du ville da skrive følgende:

```Elixir
tekst = "Hei, dette er en test"
```

For å søke og erstatte i Elixir bruker vi funksjonen `replace/3`. La oss nå bruke denne funksjonen for å erstattte "hei" med "hallo" i teksten vår:

```Elixir
ny_tekst = replace(tekst, "hei", "hallo")
```

Nå vil variabelen `ny_tekst` inneholde "Hallo, dette er en test". Som du ser, blir ordet "hei" erstattet med "hallo" i teksten.

Men hva om vi ønsker å gjøre dette med flere forekomster av et ord i teksten? Da kan vi bruke funksjonen `replace_all/3`, som vil erstatte alle forekomster av ordet vi søker etter. Her er et eksempel:

```Elixir
tekst = "Hei, dette er en test. Hei, dette er en annen test."
ny_tekst = replace_all(tekst, "hei", "hallo")
```

Nå vil `ny_tekst` inneholde "Hallo, dette er en test. Hallo, dette er en annen test."

## Dypdykk

I Elixir er det også mulig å bruke regulære uttrykk for å søke og erstatte tekst. Dette gir deg enda mer fleksibilitet og kraft når du jobber med tekstbehandling. Her er et eksempel på hvordan du kan bruke et regulært uttrykk i `replace/3`:

```Elixir
tekst = "Elixir er et fantastisk programmeringsspråk!"
ny_tekst = replace(tekst, ~r/et fantastisk/, "det beste")
```

Nå vil `ny_tekst` inneholde "Elixir er det beste programmeringsspråket!".

Det er også verdt å nevne at Elixir har mange andre nyttige funksjoner for tekstbehandling, som for eksempel `split/2`, `join/2` og `split_at/3`. Utforsk disse funksjonene og se hva du kan gjøre med dem!

## Se også

- Offisiell Elixir dokumentasjon for `replace/3`: https://hexdocs.pm/elixir/String.html#replace/3
- Elixirforumets diskusjon om søking og erstatting i tekst: https://elixirforum.com/t/text-search-replace
- ElixirCast-podcast om tekstbehandling i Elixir: https://elixircast.com/episode/10-text-processing/