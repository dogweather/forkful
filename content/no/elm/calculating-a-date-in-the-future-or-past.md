---
title:    "Elm: Beregning av datoer i fremtiden eller fortiden"
keywords: ["Elm"]
---

{{< edit_this_page >}}

#Hvorfor

Å beregne en dato i fremtiden eller fortiden kan være nyttig for å planlegge hendelser eller for å holde oversikt over datoer. Med Elm kan du enkelt lage en funksjon som kan utføre dette på en enkel og pålitelig måte.

#Slik gjør du det

For å beregne en dato i fremtiden eller fortiden, kan du bruke Elm-pakken "jasonmorgan/date-extra". Denne pakken inneholder funksjoner som gjør det enkelt å manipulere datoer. La oss se på et eksempel:

```
Elm
import Date.Extra
import Time

Date.Extra.daysSince Today (Time.millisToPosix 1644097200000)

-- Output: 11555
```

I dette eksempelet bruker vi funksjonen "daysSince" som tar inn to datoer som argumenter. I dette tilfellet bruker vi "Today" som den første datoen og en posixtid for 15. februar 2022 som den andre datoen. Som output får vi antall dager siden 15. februar 2022.

#Dykk dypere

En nyttig funksjon i "date-extra" pakken er "add". Denne funksjonen lar deg legge til et gitt antall dager, måneder eller år til en gitt dato. La oss se på et eksempel hvor vi legger til 3 måneder til dagens dato:

```
Elm
import Date.Extra
import Time

Date.Extra.add Date.Extra.Month 3 Today

-- Output: 2019-11-19
```

Som du kan se, returnerer funksjonen en ny dato som er 3 måneder senere enn dagens dato. Dette er nyttig for å planlegge fremtidige hendelser eller for å holde oversikt over datoer.

#Se også

- [Elm dokumentasjon om datoer](https://package.elm-lang.org/packages/elm/time/latest/)
- [jasonmorgan/date-extra pakken](https://package.elm-lang.org/packages/jasonmorgan/date-extra/latest/)