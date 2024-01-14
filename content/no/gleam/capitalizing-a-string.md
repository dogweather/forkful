---
title:                "Gleam: The title of an article: Stor bokstaving av en streng"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

##Hvorfor
Det kan være flere grunner til å ville kapitalisere en streng i programmering. En vanlig årsak kan være å få en mer estetisk og lesbar tekst. Dette kan være spesielt nyttig når man jobber med brukergrensesnitt eller utskrifter til konsollen. I tillegg kan det være nødvendig å formatere data slik at den følger bestemte konvensjoner, for eksempel når man sender data til et API.

##Slik gjør du det
For å kapitalisere en streng i Gleam, kan du bruke den innebygde funksjonen `String.to_uppercase`. Se et eksempel nedenfor:

```Gleam
let tekst = "gleam programming er gøy"
let kapitalisert_tekst = String.to_uppercase(tekst)
```

Dette vil gi følgende resultat:

```
"GLEAM PROGRAMMING ER GØY"
```

Du kan også formatere lengre strenger ved å iterere gjennom ordene og kapitalisere den første bokstaven i hvert ord. Dette kan gjøres ved hjelp av `String.split` og `List.map` funksjoner. Her er et eksempel:

```Gleam
let setning = "python er et populært programmeringsspråk"
let kapitalisert_setning =
    setning
    |> String.split(" ")  // Deler strengen ved mellomrom for å få en liste med ord
    |> List.map(String.to_uppercase) // Mapper hver streng til å bli kapitalisert
    |> String.join(" ") // Slår strengene sammen igjen
```

Dette vil produsere følgende resultat:

```
"PYTHON ER ET POPULÆRT PROGRAMMERINGSSPRÅK"
```

##Dypdykk
Hvis du ønsker å dypere forståelse av hvordan kapitalisering av strenger fungerer i Gleam, kan du se på hvordan `String.to_uppercase` funksjonen er implementert. Denne funksjonen bruker Unicode "character properties" for å bestemme hvilke bokstaver som skal bli kapitalisert. Det betyr at denne funksjonen også fungerer for ikke-vestlige språk som bruker andre alfabet enn det latinske.

##Se også
- Offisiell Gleam dokumentasjon for `String` modulen: https://gleam.run/modules/string
- En artikkel om Unicode character properties: https://en.wikipedia.org/wiki/Unicode_character_property