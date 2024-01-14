---
title:    "Gleam: Å bruke regulære uttrykk"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Hvorfor

Regulære uttrykk kan være en kraftig verktøy for å finne og manipulere tekst i programmering. Ved å lære å bruke dem vil du kunne skrive mer effektiv kode og løse komplekse problemer på en mer elegant måte.

## Hvordan

For å bruke regulære uttrykk i Gleam må du importere "re" biblioteket. Deretter kan du bruke funksjonene "matches" og "replace" for å finne og erstatte mønstre i en tekststreng.

```Gleam
import re

let tekst = "I dag er det en fin dag for å programmere"

let resultat = re.matches("fin", tekst)  // Resultatet blir ["fin"]
let ny_tekst = re.replace("fin", "fantastisk", tekst) // Resultatet blir "I dag er det en fantastisk dag for å programmere"
```

## Dypdykk

For å utnytte kraften til regulære uttrykk fullt ut, er det viktig å forstå forskjellige syntaks og meta-tegn som kan brukes for å matche forskjellige mønstre. For eksempel kan du bruke spesielle symboler som * for å matche et hvilket som helst antall tegn eller [a-z] for å matche en hvilken som helst bokstav.

Det finnes også en rekke forskjellige metoder og funksjoner i "re" biblioteket som kan hjelpe deg med å forenkle og optimere koden din når du jobber med regulære uttrykk.

## Se også

- Offisiell dokumentasjon for "re" biblioteket i Gleam: https://gleam.run/documentation/index.html#re
- En interaktiv tutorial for å lære hvordan du bruker regulære uttrykk: https://regexone.com/
- En nettbasert regex tester for å teste uttrykkene dine: https://regexr.com/