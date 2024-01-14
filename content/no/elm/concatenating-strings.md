---
title:    "Elm: Sammenføyning av strenger"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Hvorfor

En av de viktigste aspektene ved å programmere er å kunne manipulere data på en effektiv måte. Å kombinere eller "konkatenerer" strenger er en vanlig oppgave som kan hjelpe deg med å bygge mer dynamiske og nyttige programmer. I dette innlegget vil vi se nærmere på hvordan du kan gjøre dette ved hjelp av Elm-programmeringsspråket.

## Slik gjør du det

For å konkatenerer eller kombinere flere strenger i Elm, kan vi bruke operatøren `++` eller funksjonen `String.concat`. La oss se på et eksempel:

```
Elm
taskOne : String
taskOne =
    "Gå ut med hunden "

taskTwo : String
taskTwo =
    "og mate katten"

combinedTask : String
combinedTask =
    taskOne ++ taskTwo

-- Output: "Gå ut med hunden og mate katten"
```

I dette eksemplet har vi definert tre strenger: `taskOne`, `taskTwo` og `combinedTask`. Ved hjelp av `++` operatøren har vi konkateneret `taskOne` og `taskTwo` til å danne `combinedTask` med en mellomrom mellom de to strengene.

Du kan også bruke `String.concat` funksjonen på en liste av strenger. La oss si at vi har en liste over navn og vi ønsker å generere en hilsen med alle disse navnene, separert med komma. Da kan vi gjøre følgende:

```
Elm
names : List String
names =
    ["Lars", "Anna", "Kristian", "Sara"]

hello : String
hello =
    "Hallo, " ++ String.concat ", " names

-- Output: "Hallo, Lars, Anna, Kristian, Sara"
```

Som du kan se, brukes samme konsept som i det første eksemplet, men i stedet for å konkateneret to strenger, bruker vi `String.concat` funksjonen på listen `names` med komma som separator mellom hvert navn.

## Dykk dypere

Å konkateneret strenger er en viktig del av databehandling i de fleste programmeringsspråk. I motsetning til noen andre språk, tillater Elm ikke bruk av operatører eller funksjoner som kan endre en streng direkte, noe som kan føre til forvirring. I stedet blir en ny streng opprettet hver gang en operasjon eller funksjon blir brukt på en streng.

Det er også viktig å huske på at når du jobber med flere strengetyper, for eksempel `String` og `Text`, må du konvertere dem til samme type før du kan konkateneret dem.

## Se også

- [Elm Language Guide](https://guide.elm-lang.org/)
- [Elm String API](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm List API](https://package.elm-lang.org/packages/elm/core/latest/List)