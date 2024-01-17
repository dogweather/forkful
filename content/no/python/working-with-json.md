---
title:                "Arbeid med json"
html_title:           "Python: Arbeid med json"
simple_title:         "Arbeid med json"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/working-with-json.md"
---

{{< edit_this_page >}}

Nøkkelord: Python, JSON, programmering, data

## Hva er JSON og hvorfor bruker programmere det?

JSON (JavaScript Object Notation) er et tekstbasert dataformat som brukes til å lagre og utveksle strukturert informasjon mellom applikasjoner. Det er spesielt nyttig for å lagre og håndtere store mengder data, og er mye brukt i web-applikasjoner og databaser. Programmere bruker JSON for å enkelt kunne organisere og hente ut data på en standardisert måte.

## Slik bruker du JSON i Python:

```
import json

# Konvertere JSON til Python dictionary
data = '{"navn": "Ole", "alder": 20, "hobbyer": ["fotball", "musikk", "film"]}'
python_dict = json.loads(data)

# Skrive ut en spesifikk verdi fra dictionary
print(python_dict["alder"])

# Konvertere Python dictionary til JSON
nytt_data = json.dumps(python_dict)
print(nytt_data)
```

Output:
```
20
{"navn": "Ole", "alder": 20, "hobbyer": ["fotball", "musikk", "film"]}
```

## Dypdykk i JSON:

JSON ble utviklet som et lettere og enklere alternativ til XML for å lagre og sende data mellom applikasjoner på weben. Det er et åpent format og støttes av de fleste programmeringsspråk. JSON brukes også mye i sammenheng med REST APIer for å strukturere data og gjøre det enkelt å hente ut spesifikke data.

Å jobbe med JSON i Python er enkelt og effektivt med hjelp av "json" biblioteket. Det finnes også alternativer som SimpleJSON og uJSON som tilbyr raskere ytelse for større og mer komplekse JSON-data.

## Se også:

- [jsonlæreplan](https://www.jsonlæreplan.com/)
- [Python offisiell dokumentasjon for JSON](https://docs.python.org/3/library/json.html)
- [Real Python: Intro til JSON i Python](https://realpython.com/python-json/)