---
title:                "Arbeid med yaml"
html_title:           "Gleam: Arbeid med yaml"
simple_title:         "Arbeid med yaml"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du er en programmerer som liker å jobbe med strukturerte data og konfigurasjonsfiler, så er arbeidet med YAML noe du bør begynne å vurdere. Gleam-programmeringsspråket har innebygd støtte for YAML, som gjør det enkelt å lese og skrive dette formatet i dine programmer.

## Hvordan bruke YAML i Gleam

For å begynne å bruke YAML i Gleam, må du først importere `yaml` biblioteket ved å legge til følgende linje øverst i filen din:

```Gleam
import yaml
```

Dette lar Gleam-programmet vite at det vil bruke funksjoner og datatyper fra `yaml` biblioteket.

Nå kan vi begynne å jobbe med YAML ved hjelp av `yaml` modulen. La oss først se på et eksempel på en YAML-fil:

```YAML
name: Jane
age: 25
favorite_foods: 
  - pizza
  - tacos
  - sushi
```

For å lese denne filen i Gleam, kan vi bruke `yaml.from_string` funksjonen, som tar inn en tekststreng og returnerer en liste over tuple som representerer YAML-dataene. La oss se på et eksempel:

```Gleam
let input = "name: Jane
age: 25
favorite_foods:
- pizza
- tacos
- sushi"
let result = yaml.from_string(input)
assert result == [("| name", "Jane"), ("| age", 25), ("| favorite_foods", ["pizza", "tacos", "sushi"])]
```

Vi kan også bruke `yaml.to_string` funksjonen for å konvertere Gleam-data til YAML-format. Her er et eksempel:

```Gleam
let input = [("| name", "Jane"), ("| age", 25), ("| favorite_foods", ["pizza", "tacos", "sushi"])]
let result = yaml.to_string(input)
assert result == "name: Jane
age: 25
favorite_foods:
- pizza
- tacos
- sushi"
```

## Dykk dypere

Nå som vi har sett på et enkelt eksempel, la oss dykke dypere inn i hvordan YAML fungerer i Gleam.

I Gleam, er YAML-data representert som en `yaml.Value`-type, som kan være enten en tuple, en liste, et streng, et tall eller `yaml.Null`. Dette er viktig å huske når du leser og skriver YAML-data i dine Gleam-programmer.

En ting som er verdt å merke seg er at `yaml.Null`-type er inkludert i alle lister, så du må håndtere dette når du behandler og konverterer YAML-data.

## Se også

- [YAML-spesifikasjonen](https://yaml.org/spec/)
- [Gleam dokumentasjon om YAML](https://gleam.run/documentation/)
- [Offisiell hjemmeside for Gleam](https://gleam.run/)