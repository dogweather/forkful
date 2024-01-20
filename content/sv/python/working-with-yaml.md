---
title:                "Arbete med YAML"
html_title:           "Arduino: Arbete med YAML"
simple_title:         "Arbete med YAML"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## Vad & Varför?
YAML är ett format för data-serialisering, ofta använt för konfigurationsfiler. Programmerare använder YAML eftersom det är lättläst för människor och enkelt att parsa med datorer.

## Hur gör man:
```Python
import yaml

# Läs YAML från fil
with open('config.yaml', 'r') as file:
    data = yaml.safe_load(file)
print(data)

# Skriv YAML till fil
data_to_save = {'name': 'Olle', 'occupation': 'Developer'}
with open('new_config.yaml', 'w') as file:
    yaml.dump(data_to_save, file)
```
Exempel på `config.yaml`:
```YAML
name: Olle
occupation: Developer
```
Exempel på utskrift:
```
{'name': 'Olle', 'occupation': 'Developer'}
```

## Fördjupning
YAML, "YAML Ain't Markup Language", startades tidigt 2000-tal. Alternativ inkluderar JSON och XML som är mer verbösa. YAML parsas genom bibliotek som PyYAML i Python, där `safe_load` används för att förebygga exekvering av skadlig kod.

## Se även
- [Officiell YAML-webbplats](https://yaml.org)
- [PyYAML-dokumentation](https://pyyaml.org/wiki/PyYAMLDocumentation)
- [YAML vs JSON jämförelse](https://en.wikipedia.org/wiki/JSON#Comparison_with_YAML)