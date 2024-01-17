---
title:                "Å jobbe med yaml"
html_title:           "Python: Å jobbe med yaml"
simple_title:         "Å jobbe med yaml"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
YAML er et tekstbasert dataformat som brukes for å lagre og legge til rette for utveksling av data mellom ulike programmeringsspråk. Det er spesielt nyttig for å lagre konfigurasjonsfiler eller for å utveksle data mellom applikasjoner. 

Programmerere bruker YAML fordi det er en enkel og leselig måte å strukturere og organisere data på. Det tillater også enkel serialisering og deserialisering av data, noe som er viktig for å kunne kommunisere med andre programmer.

## Hvordan:
```Python
import yaml

# Opprette en YAML-fil
data = {"navn": "Ole", "alder": 30, "hobbyer": ["programmering", "fotografering"]}

with open("min_fil.yml", "w") as f:
    yaml.dump(data, f)

# Lese en YAML-fil
with open("min_fil.yml") as f:
    data = yaml.load(f)
    
print(data["navn"]) # Output: Ole
print(data["alder"]) # Output: 30
print(data["hobbyer"][0]) # Output: programmering
```

## Dypdykk:
YAML står for "YAML Ain't Markup Language" og ble utviklet på begynnelsen av 2000-tallet for å erstatte XML som et mer leselig og brukervennlig alternativ for å definere strukturerte data. Det finnes også andre formater som JSON og TOML, men YAML er mer fleksibelt og tillater blant annet kommentarer i filene.

For å bruke YAML i Python, må man installere pyyam biblioteket ved hjelp av en pakkehåndterer som pip eller conda. Det finnes også flere GUI-verktøy som gjør det enkelt å redigere YAML-filer.

## Se også:
Offisiell dokumentasjon for YAML: https://yaml.org/ 

Pyyaml biblioteket: https://pyyaml.org/