---
title:                "Å jobbe med yaml"
html_title:           "Gleam: Å jobbe med yaml"
simple_title:         "Å jobbe med yaml"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
YAML er en tekstbasert format for å representere og lagre datastrukturer. Det er brukt av programmører for å enkelt lese, lagre og dele data som ikke kan håndteres av andre formater som JSON eller XML.

## Hvordan:
For å arbeide med YAML i Gleam, må du først importere modulen `gleam_yaml` og deretter bruke funksjonene i denne modulen. For å lese en YAML-fil, kan du bruke `gleam_yaml.read_file`-funksjonen og angi filbanen som en streng. Hvis du vil konvertere YAML-data til Gleam-strenger eller moduler, kan du bruke `gleam_yaml.dumps` og `gleam_yaml.dumps_module`-funksjonene. Under er et enkelt eksempel på å lese en YAML-fil og skrive ut resultatet:

```
Gleam import gleam_yaml
YAML_DATA <- gleam_yaml.read_file("min_datafil.yml")
gleam_yaml.dumps(YAML_DATA) // => "min_datafil.yml"
```

## Dypdykk:
YAML ble utviklet i 2001 av Clark Evans og Ingy döt Net for å være et menneskelesbart og konsist dataformat. Det har blitt et populært valg for å representere komplekse datastrukturer i programmeringsspråk som Python, Ruby og nå også Gleam. Alternativene til YAML inkluderer JSON og XML, men YAML har en mer menneskelesbar syntaks og støtter kommentarer og referanser til andre deler av YAML-filen. I Gleam er YAML-implementasjonen basert på en parser som bruker en tilpasset versjon av `libyaml`-biblioteket.

## Se også:
- [YAML offisiell nettside](https://yaml.org)
- [YAML dokumentasjon](https://docs.ansible.com/ansible/latest/reference_appendices/YAMLSyntax.html)