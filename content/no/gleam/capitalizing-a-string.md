---
title:                "Stor bokstavføring av streng."
html_title:           "Gleam: Stor bokstavføring av streng."
simple_title:         "Stor bokstavføring av streng."
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Hva & hvorfor?
Å kapitalisere en streng i programmering betyr å gjøre den første bokstaven stor. Dette er nyttig for å gjøre en streng mer leselig for mennesker, og også for å følge vanlige navneregler. Programmerere gjør dette for å sikre konsekvent lesbarhet og konsistens i sin kode.

# Hvordan:
```Gleam
let streng1 = "hallo verden"
let kapitalisert = String.capitalize(streng1)
// Resultat: "Hallo verden"
```
```Gleam
let streng2 = "en annen streng"
let kapitalisert = String.capitalize(streng2)
// Resultat: "En annen streng"
```

# Dypdykk:
Historisk sett har å kapitalisere strenger vært en del av konvensjonen i mange programmeringsspråk, og brukes fortsatt i dag. Alternativet til å bruke en innebygd funksjon som String.capitalize i Gleam er å manuelt endre hver streng som trenger å bli kapitalisert, noe som kan føre til mer kode og potensielt flere feil. I Gleam, og mange andre språk, er kapitalisering av en streng en standardfunksjon som er enkel å implementere takket være brukervennlige biblioteker.

# Se også:
- [Gleam sin offisielle dokumentasjon om strenger](https://gleam.run/documentation/the_gleam_language/strings.html)
- [En sammenligning av strengfunksjonalitet i forskjellige programmeringsspråk](https://github.com/hoverbear/rust-rosetta#strings)