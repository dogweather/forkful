---
title:                "Python: Arbeid med yaml"
simple_title:         "Arbeid med yaml"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hvorfor
YAML (YAML Ain't Markup Language) er et populært filformat for å konfigurere og lagre datastrukturer. Det brukes ofte i programmering og konfigurasjonsfilene til ulike applikasjoner. Å jobbe med YAML gjør det enkelt å lese og forstå dataene, og det er en effektiv måte å organisere informasjonen på.

## Hvordan
For å jobbe med YAML i Python, må du først importere biblioteket "yaml". Deretter kan du bruke "yaml.load()" funksjonen for å lese YAML-filen og lagre dataene i en variabel. Nedenfor er et eksempel på hvordan du kan lese og skrive data fra en YAML-fil i Python:

```Python
import yaml 

# Leser data fra YAML-fil
with open("data.yml", 'r') as file:
    data = yaml.load(file)

# Skriver data til en YAML-fil
with open("output.yml", 'w') as file:
    yaml.dump(data, file)
```

Det finnes også andre nyttige funksjoner som "yaml.dump()", som lar deg lage en YAML-fil fra eksisterende data og "yaml.safe_load()" som beskytter mot potensielle sikkerhetshull.

## Dypdykk
I tillegg til å kunne lese og skrive YAML-filer, kan du også jobbe med komplekse datastrukturer som lister og dictionaries. YAML gjør det lett å representere disse datastrukturene og deretter trekke dem ut og bruke dem i programmet ditt. Du kan også bruke nøkkelverdier for å organisere og få tilgang til dataene dine, noe som gjør koden mer lesbar og enkel å vedlikeholde.

## Se også
- [Offisiell YAML-spesifikasjon](https://yaml.org/spec/1.2/spec.html)
- [Python yaml bibliotek](https://pyyaml.org/wiki/PyYAMLDocumentation)
- [YAML-tutorial for nybegynnere](https://rollout.io/blog/yaml-tutorial-everything-you-need-get-started/)