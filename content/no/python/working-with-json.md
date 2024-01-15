---
title:                "Å arbeide med json"
html_title:           "Python: Å arbeide med json"
simple_title:         "Å arbeide med json"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/working-with-json.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvorfor bry seg om JSON? Det er fordi JSON er et viktig verktøy for å håndtere data i moderne programmering. Det er enkelt å lese og skrive, og støttes av de fleste programmeringsspråk og APIer.

## Hvordan
Å jobbe med JSON i Python er ganske enkelt. Først må vi importere `json` biblioteket:

```Python
import json
```

For å lese en JSON-fil og konvertere den til et Python-dikt, kan vi bruke `load` -funksjonen:

```Python
with open('data.json') as f:
    data = json.load(f)
```

Nå kan vi få tilgang til dataene ved hjelp av nøklene i diktet. For eksempel, hvis dataene våre inneholder en liste over navn, kan vi skrive ut det første navnet ved å bruke `[0]` indeksen:

```Python
print(data['names'][0])
```
Output: "Jens"

For å skrive dataene våre til en JSON-fil, kan vi bruke `dump` -funksjonen:

```Python
data = {"name": "Mia", "age": 25}
with open('output.json', 'w') as f:
    json.dump(data, f)
```

Vi kan også formatere JSON-filen for å gjøre den mer lesbar ved å legge til `indent` argumentet:

```Python
json.dump(data, f, indent=4)
```

## Deep Dive
JSON står for "JavaScript Object Notation" og er en vanlig måte å strukturere data på. Den bruker nøkkel-verdi-par for å organisere informasjon i en lesbar og kompakt måte. JSON støtter forskjellige typer data, inkludert tekst, tall, booleans, lister og dikt.

I Python, kan vi bruke `json.loads` -funksjonen for å konvertere en JSON-streng til et Python-dikt:

```Python
json_string = '{"name": "Anne", "age": 30}'
data = json.loads(json_string)
```

Vi kan også pakke data i en JSON-streng ved å bruke `json.dumps` -funksjonen:

```Python
json_string = json.dumps({"name": "Erik", "age": 35})
```

Nå som vi har en bedre forståelse av hva JSON er og hvordan vi kan håndtere det i Python, kan vi bruke denne kunnskapen til å bygge kraftige programmer og integrere med ulike APIer som bruker JSON-formatet.

## Se også
- [Offisiell Python JSON-dokumentasjon](https://docs.python.org/3/library/json.html)
- [w3schools guide til JSON i Python](https://www.w3schools.com/python/python_json.asp)
- [DataCamp tutorial om å jobbe med JSON-data i Python](https://www.datacamp.com/community/tutorials/json-data-python)