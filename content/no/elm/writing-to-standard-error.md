---
title:    "Elm: Skriver til standardfeil"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Hvorfor

Når du skriver kode, er det viktig å håndtere feil og feilretting på en effektiv måte. En måte å gjøre dette på er å skrive til standardfeil (standard error) i stedet for standard utgang (standard output). Dette hjelper deg med å identifisere og håndtere feil på en strukturert måte.

# Hvordan

For å skrive til standardfeil i Elm, kan du bruke funksjonen "Debug.crash" sammen med en feilmelding som parameter. Dette vil skrive ut feilmeldingen til standardfeil når programmet ditt kjører. Se eksemplet nedenfor:

```Elm
Debug.crash "Noe gikk galt!"
```

Dette vil gi følgende utgang når programmet kjøres:

```
-- ERROR ------- noe gikk galt --------
```

Dette gjør det enkelt å identifisere hvor og når en feil oppstår mens du kjører koden din.

# Dypdykk

Når du skriver til standardfeil i Elm, vil feilene dine vises i en strukturert og lett lesbar måte. Dette gjør det lettere å diagnostisere og løse problemer. I tillegg vil standardfeil også skrive ut verdifulle detaljer om kontekst og stack-trace, som kan hjelpe deg med å forstå hva som forårsaker feilen.

Å skrive til standardfeil er spesielt nyttig når du håndterer feil i produksjonsmiljøet ditt. Ved å logge feil til standardfeil, kan du enkelt spore og identifisere problemer som brukere møter, og potensielt løse dem raskere.

# Se Også

- Offisiell Elm dokumentasjon for Debug Module: https://package.elm-lang.org/packages/elm/core/latest/Debug
- Artikkel om Debugging i Elm: https://dev.to/marinantonio/debugging-in-elm-3ol0
- Elm Slack Channel: https://elmlang.slack.com/