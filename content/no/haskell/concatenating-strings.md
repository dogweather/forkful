---
title:                "Sammenføyning av strenger"
html_title:           "Haskell: Sammenføyning av strenger"
simple_title:         "Sammenføyning av strenger"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

Hva & hvorfor?
Når vi snakker om å konkatentere strenger i Haskell, så betyr det rett og slett å kombinere to eller flere strukturelt like strenger til en. Dette kan være nyttig for å lage mer komplekse uttrykk eller manipulere tekster på en mer effektiv måte.

Hvordan:
Dette kan gjøres ved å bruke operatoren "++", som tar to strenger og kombinerer dem til en. For eksempel:

```Haskell
"Hello " ++ "world"  --> "Hello world"
```

Det er viktig å merke seg at begge strengene må være av samme type for å kunne kombineres på denne måten. Ellers vil man få en typefeil.

Det er også mulig å konkatentere en liste av strenger ved å bruke funksjonen "concat". Dette gjøres ved å gi funksjonen en liste av strenger som argument, for eksempel:

```Haskell
concat ["Hello ", "world"]  --> "Hello world"
```

Dypdykk:
Konkatenering av strenger er en vanlig operasjon i mange programmeringsspråk. I Haskell, som i andre funksjonelle språk, har vi også muligheten til å bruke rekursjon for å implementere denne funksjonaliteten. Dette kan være nyttig hvis man ønsker å lage en mer kompleks funksjon som konkatenerer en ubestemt mengde strenger.

Det finnes også andre måter å få til dette på i Haskell, for eksempel ved hjelp av funksjoner som "foldr" eller ved å bruke monader. Men for de fleste tilfeller vil bruk av operatoren "++" eller funksjonen "concat" være tilstrekkelig.

Se også:
- Offisiell dokumentasjon for Haskell: https://www.haskell.org/
- Utdypende artikkel om konkatenering av strenger i Haskell: https://wiki.haskell.org/Concatenation