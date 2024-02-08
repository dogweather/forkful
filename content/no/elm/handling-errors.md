---
title:                "Feilhåndtering"
aliases:
- no/elm/handling-errors.md
date:                  2024-01-26T00:52:19.701117-07:00
model:                 gpt-4-1106-preview
simple_title:         "Feilhåndtering"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/handling-errors.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å håndtere feil betyr å skrive kode som kan forutse og håndtere når ting går galt. Utviklere gjør dette for å forhindre krasj, beskytte dataintegritet og tilby brukere smidige tilbakefallsløsninger.

## Hvordan:
Elms kjernefilosofi er Ingen Kjøretidsunntak. Så Elm utnytter sitt typesystem med typer som `Maybe` og `Result` for å håndtere feil.

For `Maybe`-scenario:

```Elm
safeDivide : Float -> Float -> Maybe Float
safeDivide numerator denominator =
    if denominator == 0 then
        Nothing
    else
        Just (numerator / denominator)
        
-- Når du kjører det:

safeDivide 10 2
--> Just 5

safeDivide 10 0
--> Nothing
```

For `Result`-scenario:

```Elm
type Error = DivisionByZero

safeDivide : Float -> Float -> Result Error Float
safeDivide numerator denominator =
    if denominator == 0 then
        Err DivisionByZero
    else
        Ok (numerator / denominator)

-- Og bruker det:

safeDivide 10 2
--> Ok 5

safeDivide 10 0
--> Err DivisionByZero
```

## Dypdykk
Elms typesystem er strengt, noe som hjelper med å fange opp feil tidlig. Historisk sett har de fleste språk stolt på unntak og kjøretidssjekker, men Elm har valgt garantiene ved kompileringstid. Alternativer som `Result` tillater detaljert feilinformasjon, mens `Maybe` er enklere for ja-nei-scenarioer. Elms feilhåndtering oppmuntrer utviklere til å vurdere alle veier på forhånd og unngår fallgruvene med glemte feiltilfeller.

## Se også:
- Elms offisielle guide om feilhåndtering: [Feilhåndtering – En Introduksjon](https://guide.elm-lang.org/error_handling/)
- Elm `Maybe`-dokumentasjon: [Elm – Maybe](https://package.elm-lang.org/packages/elm/core/latest/Maybe)
- Elm `Result`-dokumentasjon: [Elm – Result](https://package.elm-lang.org/packages/elm/core/latest/Result)
