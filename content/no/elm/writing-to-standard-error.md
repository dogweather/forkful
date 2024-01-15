---
title:                "Skriver til standard feil"
html_title:           "Elm: Skriver til standard feil"
simple_title:         "Skriver til standard feil"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive til standard error kan være nyttig når du jobber med feilhåndtering og debugging i Elm. Det lar deg skrive feilmeldinger og diagnostisk informasjon direkte til terminalen, i stedet for å måtte logge det til en fil eller konsoll.

## Hvordan

For å skrive til standard error i Elm, kan du bruke funksjonen `Debug.crash` og passere inn en melding som en streng. For eksempel:

```
import Debug exposing (..)

main =
    Debug.crash "Dette er en feilmelding"
```

Dette vil skrive ut meldingen "Dette er en feilmelding" til standard error når applikasjonen kjører.

Hvis du vil inkludere variabler eller andre data i meldingen, kan du bruke funksjonen `Debug.toString` for å konvertere dem til strenger. For eksempel:

```
import Debug exposing (..)

main =
    let
        errorCode = 404
    in
    Debug.crash ("Feil: " ++ (Debug.toString errorCode))
```

Dette vil skrive ut meldingen "Feil: 404" til standard error.

Du kan også bruke funksjonen `Debug.log` for å skrive til standard error uten å stoppe kjøringen av applikasjonen. Dette kan være nyttig når du vil logge informasjon underveis i programmet. For eksempel:

```
import Debug exposing (..)

main =
    let
        bruker = "Jon"
    in
    Debug.log "Bruker: " bruker
```

Dette vil skrive ut meldingen "Bruker: Jon" til standard error.

## Dypdykk

Det er viktig å huske at å skrive til standard error ikke alltid er den beste løsningen for feilhåndtering. På grunn av hvordan Elm håndterer feil, kan det være bedre å bruke kanskje bruke `Result` eller`Maybe` typer til å håndtere feil på en mer strukturert måte.

Du bør også være forsiktig med å skrive sensitiv informasjon til standard error, da dette kan bli fanget av feilloggingssystemer og potensielt utgjøre en sikkerhetsrisiko.

## Se også

- [Elm dokumentasjon om feilhåndtering](https://guide.elm-lang.org/error_handling/)
- [The Elm Architecture](https://guide.elm-lang.org/architecture/)