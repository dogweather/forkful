---
title:                "Gleam: Sammenstilling av strenger"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kombinere strenger (eller "streng-konkatinering") er en essensiell del av programmeringsverdenen. Det lar deg kombinere flere tekststrenger sammen for å lage en lengre streng. Dette kan være nyttig for å lage dynamiske meldinger eller setninger, samt å manipulere data og informasjon. I denne blogginnlegget skal vi utforske hvordan du kan gjøre dette i Gleam programmeringsspråk.

## Hvordan

For å konkatenerere strenger i Gleam, kan du bruke funksjonen "str.concat" og angi de to strengene du vil kombinere som argumenter. La oss si at vi vil kombinere strengen "Hei" med "verden", outputen vil da bli "Hei verden". Her er et eksempel på hvordan det ville se ut skrevet ut i Gleam-kode:

```Gleam
let greeting = str.concat("Hei", "verden")
io.print("Output: #{greeting}")
```

Output:

```
Output: Hei verden
```

Som du kan se, ble de to strengene kombinert og resultatet ble skrevet ut i konsollen.

## Dypdykk

I Gleam, som i mange andre programmeringsspråk, kan du også konkatenerere strenger ved å bruke "+" operatøren. For eksempel:

```Gleam
let greeting = "Hei" + "verden"
io.print("Output: #{greeting}")
```

Output:

```
Output: Hei verden
```

Det er også verdt å nevne at du kan kombinere flere strenger samtidig ved å bruke funksjonen "str.concat_many". Denne funksjonen tar en liste med strenger som argument og kombinerer dem alle sammen. Her er et eksempel:

```Gleam
let greetings = ["Hei", "Hallo", "God dag"]
let combined_greetings = str.concat_many(greetings)
io.print("Output: #{combined_greetings}")
```

Output:

```
Output: HeiHalloGod dag
```

## Se også

- Offisiell Gleam dokumentasjon: https://gleam.run/documentation/
- Gleam tutorials og eksempler: https://github.com/gleam-lang/gleam/tree/main/examples