---
title:                "Gleam: Arbeide med yaml"
simple_title:         "Arbeide med yaml"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

# Hvorfor

Hvis du er en programmerer som jobber med YAML-filer, har du kanskje støtt på utfordringer ved å manuelt endre og oppdatere disse filene. Det kan være tidkrevende og ofte resultere i feil. Derfor kan det være lurt å vurdere å lære å programmere med YAML, spesielt hvis du har store YAML-filer som trenger hyppige endringer.

# Hvordan

Gleam er et programmeringsspråk som gjør det enkelt å jobbe med YAML-filer. La oss se på et eksempel på hvordan vi kan bruke Gleam til å lese og oppdatere en YAML-fil.

```
Gleam import yaml
let yaml_fil = "navn: Jane\nalder: 25\n"
let data = yaml.parse(yaml_fil)
let ny_alder = 30
let oppdatert_data = {data | ..{| "alder" => ny_alder |}}
let ny_yml_fil = yaml.stringify(oppdatert_data)
```

I dette eksempelet bruker vi YAML-modulen i Gleam for å først lese inn en YAML-fil og deretter oppdatere alderen til personen som er angitt i filen. Deretter konverterer vi dataene tilbake til YAML-format og får en oppdatert YAML-fil.

## Deep Dive

Gleam sin YAML-modul støtter også muligheten til å lage nye YAML-filer og slette eksisterende oppføringer. Du kan også bruke modulen til å validere YAML-filer for å sikre at de følger riktig syntaks. Dette gjør det enklere å jobbe med komplekse YAML-strukturer og sikrer at filene dine er korrekte og klare for videre behandling.

# Se også

- [Gleam sin offisielle nettside](https://gleam.run/)
- [YAML-dokumentasjon](https://yaml.org/)