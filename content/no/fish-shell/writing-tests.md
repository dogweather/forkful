---
title:                "Fish Shell: Skriving av tester"
programming_language: "Fish Shell"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor
Tester er en integrert del av ethvert programmeringsprosjekt, uavhengig av programmeringsspråk eller plattform. De bidrar til å sikre koden din, fange feil og bugs, og forbedre den generelle kvaliteten på programvaren din. Å skrive tester kan ta litt ekstra tid, men det sparer deg for mye tid og frustrasjon i det lange løp.

## Hvordan gjøre det
For å skrive tester i Fish Shell, kan du bruke den innebygde `assert`-kommandoen. Denne kommandoen tar inn en betingelse og en feilmelding og vil bare gi en feil hvis betingelsen ikke er oppfylt. La oss ta en titt på et eksempel:

```Fish Shell
set var "Hei"

assert "Hei" = "$var" "Variabelen er ikke satt til 'Hei'"
```

I dette tilfellet sjekker vi om variabelen `var` er satt til strengen "Hei". Hvis den ikke er det, vil testen feile og gi feilmeldingen "Variabelen er ikke satt til 'Hei'". Du kan også bruke `assert` for å sjekke om kommandoen din gir den forventede utgangen, for eksempel:

```Fish Shell
set output (echo "Hei")

assert "Hei" = "$output" "Utgangen er ikke det forventede"
```

Disse er bare noen få eksempler, og du kan bruke `assert` til å teste nesten alle typer betingelser og utganger.

## Dypdykk
Å skrive gode tester handler ikke bare om å bruke riktig kommando, men også om å skrive effektive tester som dekker alle mulige tilfeller. En god praksis er å lage forskjellige tester for suksess og feiltilfeller. Du kan også bruke `not`-operatøren for å teste for negative scenarier, for eksempel:

```Fish Shell
set var 10

assert not "5" -lt "$var" "Variabelen er mindre enn 5"
```

Denne testen vil bare passere hvis variabelen `var` er større eller lik 5.

Det er også viktig å organisere og strukturere testene dine på en måte som gjør det enkelt å finne og feilsøke dem. Du kan bruke `describe` og `it` kommandoene for å organisere testene dine i logiske grupper og gi dem beskrivende navn.

## Se også
- [Fish Shell dokumentasjon om å skrive tester](https://fishshell.com/docs/current/#writing-tests)
- [GitHub-siden til Fish Shell](https://github.com/fish-shell/fish-shell)
- [En guide til å skrive effektive tester i Fish Shell](https://medium.com/@fantasticfiasco/fish-shell-testing-45f52c5f7561)