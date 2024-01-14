---
title:                "Elm: Å få gjeldende dato"
simple_title:         "Å få gjeldende dato"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Det er viktig å kunne få tak i den aktuelle datoen når du programmerer i Elm, spesielt hvis du jobber med applikasjoner som trenger å vise eller håndtere datoer på en nøyaktig måte. Dette kan være alt fra kalendere og timeplanleggere til nettbutikker som må holde styr på leveringsdatoer. Uansett hva slags applikasjon du utvikler, er det nyttig å kunne få tak i den korrekte datoen raskt og enkelt.

## Hvordan gjøre det

Å få tak i den aktuelle datoen i Elm er ganske enkelt. Du kan bruke funksjonen `Date.now` for å få datoen og klokkeslettet i millisekunder. Dette kan deretter konverteres til en `Date`-verdi ved hjelp av funksjonen `Date.fromTime`. For eksempel:

```Elm
Date.fromTime (Date.now model)
Date.fromTime 1621623304109
```

Dette vil returnere en `Date`-verdi som du kan bruke videre i koden din.

## Dykk dypere

Hvis du vil utforske mer avanserte måter å håndtere og manipulere datoer på i Elm, kan du bruke biblioteket [elm-time](https://package.elm-lang.org/packages/elm/time/latest/) som gir deg flere funksjoner og verktøy for å håndtere tid og datoer. Du kan også bruke [Date Picker](https://github.com/elm-community/date-picker) for å lage brukervennlige kalendere og velgere for datoer i applikasjonen din.

## Se også

- [elm-time bibliotek](https://package.elm-lang.org/packages/elm/time/latest/)
- [Date Picker bibliotek](https://github.com/elm-community/date-picker)