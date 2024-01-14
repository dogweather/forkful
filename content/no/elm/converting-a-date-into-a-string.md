---
title:                "Elm: Konvertering av dato til streng"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Når du arbeider med datoen, er det ofte nødvendig å konvertere den fra et datoobjekt til en streng som kan vises for brukeren. Dette gjelder spesielt i webutvikling og applikasjonsutvikling. I denne blogginnlegget vil jeg gå gjennom hvordan du kan konvertere en dato til en streng i Elm-programmering.

## Hvordan

For å konvertere en dato til en streng i Elm-programmering, kan du bruke funksjonen `Date.toFormattedString` som tar inn et `Date`-objekt og returnerer en streng i det ønskede formatet. La oss se på et eksempel:

```Elm
import Date exposing (Date)


myDate : Date
myDate = Date.fromCalendarDate 2021 6 14

-- Konverterer datoen til en streng i formatet "dd.mm.yyyy"
convertedDate = Date.toFormattedString "%d.%m.%Y" myDate

{- Output: "14.06.2021" -}
```

Som du kan se, tar `Date.toFormattedString` inn to argumenter - et formatstring og en dato og returnerer strengen i ønsket format. Formatstringen følger standardene til `strftime` i C-språk. Du kan også bruke funksjonen `Date.fromString`, som tar inn en streng og returnerer et `Date`-objekt.

## Dypdykk

Når du bruker `Date.fromFormattedString`-funksjonen, er det viktig å være klar over formatvariantene som støttes av denne funksjonen. Du kan finne en fullstendig liste over formatvariantene på Elm-dokumentasjonssiden for `Date`-modulen. Noen andre ting å huske på når du konverterer en dato til en streng er:

- Datostørrelsen, som angir om datoen skrives ut med to eller fire sifre for å representere året.
- Forventet format for månedsnavn og ukedagsnavn, som kan variere basert på språk.
- Om du ønsker å inkludere tidsinformasjon i strengen, eller bare dato.

## Se Også

- [Elm-docs: Modulen `Date`](https://package.elm-lang.org/packages/elm/date/latest/)
- [Eksempel på å bruke `Date`-modulen i Elm-språket](https://elmprogramming.com/snippets/elm/date)