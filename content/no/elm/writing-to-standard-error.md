---
title:    "Elm: Skriver til standardfeil"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Hvorfor

Det kan være mange grunner til å skrive til standard error under programmering. Det kan være nyttig for debugging og feilsøking, og kan også hjelpe med å identifisere potensielle problemer i koden din.

## Hvordan gjøre det

Å skrive til standard error i Elm er enkelt og kan gjøres ved å bruke funksjonen `Debug.crash`. For eksempel, hvis du vil skrive ut en feilmelding, kan du gjøre følgende:

```Elm
Debug.crash "Det har oppstått en feil."
```

Dette vil resultere i at `"Det har oppstått en feil."` blir skrevet ut i standard error.

## Dypdykk

Å skrive til standard error kan også være nyttig når du trenger å se på verdier eller variabler under kjøring av programmet ditt. For å gjøre dette, kan du bruke funksjonen `Debug.log`. For eksempel, hvis du vil se på verdien av en variabel `x`, kan du gjøre følgende:

```Elm
Debug.log "Variabel x" x
```

Dette vil skrive ut `"Variabel x"` og verdien av `x` i standard error. Dette kan være til hjelp når du prøver å finne ut hvorfor koden din ikke fungerer som forventet.

## Se også

- Offisiell Elm dokumentasjon for Debug: [https://package.elm-lang.org/packages/elm/core/latest/Debug](https://package.elm-lang.org/packages/elm/core/latest/Debug)
- En gjennomgang av debugging i Elm: [https://guide.elm-lang.org/debugging/](https://guide.elm-lang.org/debugging/)
- Elm live programmering fra Interaktiv Elm-bloggen: [https://terezka.github.io/interaktiv-elm/](https://terezka.github.io/interaktiv-elm/)