---
title:    "Elm: Konvertere en dato til en streng"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor
Å konvertere en dato til en streng er en vanlig oppgave når man jobber med webutvikling. Dette kan være nyttig for å vise datoen på en mer lesbar måte eller for å lagre datoen i en database. I denne bloggposten vil jeg vise deg hvordan du kan gjøre dette enkelt ved å bruke Elm-programmeringsspråket.

## Hvordan gjøre det

For å konvertere en dato til en streng i Elm, må du først importere DateTime-pakken ved å legge til følgende linje øverst i fila:

```Elm
import DateTime
```

Deretter kan du bruke funksjonen `toString` fra DateTime-modulen for å konvertere datoen til en streng. Her er et eksempel på hvordan du kan gjøre dette:

```Elm
DateTime.toString DateTime.sunday -- "Søndag"
```

Du kan også spesifisere ønsket format for datoen ved å bruke funksjonen `fromDate`. Her er et eksempel der datoen er satt til 1. januar 2021 og formatet er satt til "dd/mm/åååå":

```Elm
DateTime.fromDate 2021 1 1
  |> Result.map (DateTime.format "dd/mm/åååå") -- "01/01/2021"
```

Som du kan se, er det ganske enkelt å konvertere en dato til en streng ved hjelp av Elm.

## Dypdykk
Dersom du ønsker å lære mer om hvordan Elm behandler datoer og tider, kan du sjekke ut DateTime-pakken sin dokumentasjon på [Elm Packages](https://package.elm-lang.org/packages/justinmimbs/time-extra/latest/DateTime). Her finner du en oversikt over alle funksjonene som er tilgjengelige for å manipulere og konvertere datoer og tider.

## Se også
- [Elm Packages](https://package.elm-lang.org/)
- [Offisiell Elm dokumentasjon](https://guide.elm-lang.org/)