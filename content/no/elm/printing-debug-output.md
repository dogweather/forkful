---
title:                "Utskrift av feilsøkingsresultater"
html_title:           "Elm: Utskrift av feilsøkingsresultater"
simple_title:         "Utskrift av feilsøkingsresultater"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Printing av debug-utdata er en vanlig praksis blant programmerere for å hjelpe med å feilsøke og finne feil i koden deres. Dette kan være nyttig når man jobber med komplekse programmer eller når man har problemer med å identifisere og løse bugs.

## Hvordan:
For å skrive ut debug-utdata i Elm, bruker man funksjonen `Debug.log` og gir den en streng som beskriver den utdataen man vil skrive ut, og en verdi som skal skrives ut.

```
Elm.funksjon |> Debug.log "Beskrivelse av utdata" |> Debug.toString
```

Eksempel på utdata:

```
Beskrivelse av utdata 55
```

## Dypdykk:
Denne praksisen med å skrive ut debug-utdata stammer fra den første tiden med programmering, da det var den eneste måten å inspisere og forstå programmet på. I dag finnes det mer avanserte verktøy og metoder for å feilsøke, som gjør denne praksisen mindre vanlig. En annen mulighet i Elm er å bruke `Debugger`-modulen til å inspisere programmet og se på alle variablene i sanntid.

## Se også:
- [Elm sin offisielle dokumentasjon om debug-utdata](https://package.elm-lang.org/packages/elm/core/latest/Debug)
- [En guide til debugging i Elm](https://medium.com/@z5h/understanding-debugging-in-elm-26ee168ab265)