---
title:                "Analyse av dato fra en streng"
html_title:           "Gleam: Analyse av dato fra en streng"
simple_title:         "Analyse av dato fra en streng"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

Hva og hvorfor?
Parsing av datoer fra en streng betyr å konvertere en tekstbasert dato til et datobjekt som datamaskinen kan forstå og manipulere. Programmere gjør dette for å enkelt kunne behandle og organisere datoer i sine programmer.

Hvordan:

Gleam har en innebygd modul kalt Time som gjør det enkelt å parse datoer fra en streng. Dette gjøres ved å bruke funksjonen ```Time.format``` og spesifisere formatet på datoen i strengen. Se et eksempel under:

```
let str_date = "10/03/2021"
let date = Time.format("%d/%m/%Y", str_date)
```

Dette vil konvertere strengen "10/03/2021" til et datobjekt som kan brukes i programmet. Output vil være som følger:

```
date = { day: 10, month: 03, year: 2021 }
```

Dypdykk:
Parsing av datoer har vært en utfordring for programmerere i lang tid, da datoer kan være representert på forskjellige måter i forskjellige deler av verden. Tidligere måtte programmerere håndtere dette manuelt, men med utviklingen av språk som Gleam, er det blitt mye enklere og mer nøyaktig. Alternativer til Gleam inkluderer språk som Ruby og Python, som også har innebygde funksjoner for å parse datoer fra strenger.

Når det kommer til implementasjon, bruker Gleam Time-modulen et system av formater som gjør det mulig å konvertere datoer fra forskjellige strengformater. Dette gjør parsingen mer fleksibel og nøyaktig.

Se også:
Hvis du ønsker å lære mer om bruk av datoer i Gleam, kan du se dokumentasjonen for Time-modulen her: https://gleam.run/modules/time.html. Du kan også dra nytte av å se på andre språk som også tilbyr lignende funksjonalitet for parsing av datoer, for eksempel Ruby og Python.