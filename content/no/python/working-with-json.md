---
title:                "Python: Å jobbe med json"
simple_title:         "Å jobbe med json"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/working-with-json.md"
---

{{< edit_this_page >}}

## Hvorfor

JSON (JavaScript Object Notation) er et populært dataformat som brukes til å lagre og utveksle data. Det er spesielt nyttig i Python-programmering fordi det enkelt kan konverteres til og fra datatyper i språket, og det er kompatibelt med mange ulike programmeringsspråk. Ønsker du å lære hvordan du kan jobbe med JSON i Python? Fortsett å lese!

## Hvordan gjøre det

Det første vi må gjøre er å importere `json` biblioteket i Python:

```Python
import json
```

La oss så opprette en enkel JSON-streng som inneholder informasjon om en person:

```Python
person = {"navn": "Maren", "alder": 29, "yrke": "programmerer"}
```

For å konvertere denne strengen til JSON-format bruker vi `json.dumps()` funksjonen:

```Python
json_string = json.dumps(person)
```

Vi kan også gjøre det motsatte, konvertere en JSON-streng til et Python-objekt, ved å bruke `json.loads()` funksjonen:

```Python
person = json.loads(json_string)
```

Her er et eksempel på hvordan du kan jobbe med en JSON-fil i Python:

```Python
# Åpne en JSON-fil
with open("data.json", "r") as f:
    # Lese innholdet
    data = json.load(f)
    
    # Print en spesifikk verdi
    print(data["key"])
    
    # Loop gjennom alle elementer
    for item in data:
        print(item)
```

Et viktig konsept i JSON er nøkler og verdier. Nøklene er unike identifikatorer for hver verdi, og du kan skrive dem som vanlige strenger eller tall. Verdiene kan være av hvilken som helst datatypes, som for eksempel strenger, tall, lister eller dictionaries.

## Dykk dypere

En dypere forståelse av JSON er viktig for å kunne jobbe effektivt med det. Her er noen tips for å dykke dypere inn i dette emnet:

- Lær mer om de ulike datatypene som støttes av JSON og hvordan de konverteres til Python-objekter.
- Utforsk biblioteker som `requests` og `urllib` for å hente data fra APIer i JSON-format.
- Bruk `json.dumps()` med argumentet `indent` for å få en mer lesbar JSON-streng.
- Ta en titt på mulighetene for å validere og formatere JSON-strenger med `jsonschema`.

## Se også

- [Python dokumentasjon for json-modulen](https://docs.python.org/3/library/json.html)
- [W3Schools guide for JSON i Python](https://www.w3schools.com/python/python_json.asp)
- [DigitalOcean tutorial for arbeid med JSON i Python](https://www.digitalocean.com/community/tutorials/how-to-work-with-json-in-python)