---
title:                "Arbeid med yaml"
html_title:           "Python: Arbeid med yaml"
simple_title:         "Arbeid med yaml"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hvorfor

YAML, forkortelse for "YAML Ain't Markup Language", er en tekstbasert format for å lagre og overføre data. Det brukes ofte til konfigurasjonsfiler og brukes i et bredt spekter av applikasjoner. YAML er en enklere og mer lesbar måte å representere data på sammenlignet med andre formater som JSON og XML.

## Hvordan

Det er enkelt å arbeide med YAML i Python ved hjelp av pyyaml biblioteket. Først må du installere biblioteket ved hjelp av pip-kommandoen:

```Python
pip install pyyaml
```

Deretter kan du importere biblioteket i Python og bruke funksjoner som yaml.load() og yaml.dump() for å lese og skrive YAML-data.

```Python
import yaml

# Les YAML-fil
with open('data.yml') as file:
    data = yaml.load(file, Loader=yaml.FullLoader)

# Skriv YAML-data til konsollen
print(yaml.dump(data))
```

Utskrift av data.yml-filen vil se slik ut:

```yaml
navn: John Doe
alder: 30
hobbyer:
  - fotball
  - friluftsliv
adresse:
  gate: Hovedgata
  nummer: 1
  postnummer: 1234
```

## Dypdykk

Siden YAML er et tekstbasert format, kan det enkelt leses og redigeres av mennesker. Syntaxen er også enklere å forstå sammenlignet med andre formater. YAML støtter også kommentarer, noe som gjør det enklere å forklare og dokumentere dataene.

I tillegg til å lese og skrive YAML-filer, kan man også konvertere data til YAML-format fra andre formater som JSON og XML.

## Se også

- [pyyaml dokumentasjon](https://pyyaml.org/wiki/PyYAMLDocumentation)
- [YAML.org](https://yaml.org/)
- [Hvorfor YAML er et nyttig format for konfigurasjonsfiler](https://dev.to/alexdovzhanyn/hvorfor-yaml-har-begynt-a-bli-et-populaert-format-for-konfigurasjonsfiler-1iie)