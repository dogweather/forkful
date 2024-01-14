---
title:    "Elm: Sammenligner to datoer"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Hvorfor

Hvis du noen gang har programmert i Elm, har du sannsynligvis støtt på et problem med å sammenligne to datoer. Dette kan virke som en enkel oppgave, men det er faktisk ganske komplisert. I denne bloggposten vil jeg gå gjennom hvorfor det er nødvendig å sammenligne to datoer og hvordan du kan gjøre det på en effektiv måte.

## Hvordan

Det finnes forskjellige metoder for å sammenligne to datoer i Elm, men den mest pålitelige og enkleste måten er ved å bruke den innebygde funksjonen `DateTime.compare`. La oss si at du har to datoer: `dato1` og `dato2`. For å sammenligne dem, kan du bruke følgende uttrykk:

```Elm
case DateTime.compare dato1 dato2 of
  LT -> "dato1 er tidligere enn dato2"
  EQ -> "dato1 og dato2 er de samme"
  GT -> "dato1 er senere enn dato2"
```

Dette vil sammenligne de to datoene og returnere en `Order` type, som kan være `LT` (mindre enn), `EQ` (lik) eller `GT` (større enn). Deretter kan du bruke en `case` uttalelse for å håndtere de forskjellige tilfellene.

## Dypdykk

Som nevnt tidligere, er det ganske komplisert å sammenligne to datoer på en nøyaktig måte. Dette er fordi datoer kan ha forskjellige tidszoner, formater og inneholde tidspunkter. Derfor er det viktig å konvertere datoene til en felles enhet før du sammenligner dem. Dette kan gjøres ved å bruke funksjonen `DateTime.toUtc`.

Det er også viktig å vite at `DateTime.compare`funksjonen sammenligner både dato og tid. Hvis du bare vil sammenligne datoen, kan du bruke funksjonen `Date.compare` i stedet.

## Se også

- Elm dokumentasjon om `DateTime` og `Date`: https://package.elm-lang.org/packages/elm/time/latest/
- En artikkel om datoer og tidsstyring i Elm: https://dev.to/s-yucel/date-and-time-management-in-elm-55g2