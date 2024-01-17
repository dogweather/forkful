---
title:                "Utvinning av understrenger"
html_title:           "Gleam: Utvinning av understrenger"
simple_title:         "Utvinning av understrenger"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Ekstrahering av substrings er en vanlig operasjon i programmering som innebærer å isolere en del av en streng basert på et gitt sett med regler eller kriterier. Dette gjøres for å håndtere tekstbaserte data mer effektivt og gjøre det enklere å manipulere og analysere dem.

## Hvordan:
Hvis du for eksempel har en streng med et fullstendig navn, kan du bruke Gleam for å trekke ut bare etternavnet ved å bruke et spesifikt tegn eller indeks som markør for hvor du vil at substringsen skal starte og slutte. I koden under viser vi et eksempel på hvordan man kan bruke ```substring``` funksjonen i Gleam for å ekstrahere etternavnet fra en fullstendig navnestreng og deretter skrive det ut til konsollen.

```Gleam
let fullt_navn = "John Doe"

let etternavn = String.substring(fullt_navn, 5, 8)

IO.println("Etternavn: " ++ etternavn)
```

Output:
```
Etternavn: Doe
```

## Dypdykk:
Ekstrahering av substrings har vært en viktig del av programmering siden begynnelsen, da mange operativsystemer brukte tegnbaserte grensesnitt. Alternativer til å bruke substring funksjoner inkluderer regex, som kan være mer fleksibel, men også være mer komplisert i form av utforming og implementering. Når det gjelder Gleam, er ```substring``` funksjonen implementert ved hjelp av standardbiblioteket, slik at du ikke trenger å installere noe annet for å bruke den.

## Se også:
- [Offisiell Gleam Dokumentasjon](https://gleam.run/documentation)
- [Github Repo for Gleam prosjektet](https://github.com/gleam-lang/gleam)