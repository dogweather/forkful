---
title:                "Ekstrahering av delstrenger"
html_title:           "Elm: Ekstrahering av delstrenger"
simple_title:         "Ekstrahering av delstrenger"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å trekke ut substrings kan være en nyttig teknikk når du jobber med tekstbehandling i Elm. Det lar deg manipulere tekstdeler og få ut spesifikke deler av en streng.

## Hvordan

For å trekke ut en substring, bruker du funksjonen `String.slice` og gir den to argumenter: start og sluttindeks for den delen av teksten du ønsker å få ut. La oss ta en titt på et eksempel:

```elm
tekst = "Dette er en eksempeltekst"
substring = String.slice 5 12 tekst
```

I dette tilfellet vil `substring` være lik "er en e".

Du kan også bruke `String.left` og `String.right` for å trekke ut tekstdeler fra henholdsvis venstre og høyre side av en streng. Her er et annet eksempel:

```elm
tekst = "Elm er et fantastisk programmeringsspråk"
venstreDel = String.left 3 tekst
høyreDel = String.right 17 tekst
```

I dette eksempelet vil `venstreDel` bli "Elm" og `høyreDel` bli "programmeringsspråk".

## Dypdykk

Hvis du vil dykke dypere inn i temaet substrings, kan du også bruke funksjonen `String.substr` for å trekke ut et bestemt antall tegn fra en gitt startindeks. Her er et eksempel på hvordan du kan bruke dette:

```elm
tekst = "Denne setningen er ganske lang"
substring = String.substr 6 8 tekst
```

Her vil `substring` være lik "setningen". Legg merke til at det første argumentet er startindeksen, og det andre argumentet er antall tegn som skal tas ut.

## Se Også

- [Elm Dokumentasjon for String-modulen](https://package.elm-lang.org/packages/elm/core/latest/String)
- [W3Schools: Substring i Elm](https://www.w3schools.com/jsref/jsref_substring.asp)