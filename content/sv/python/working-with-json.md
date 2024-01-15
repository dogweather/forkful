---
title:                "Att arbeta med json"
html_title:           "Python: Att arbeta med json"
simple_title:         "Att arbeta med json"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/working-with-json.md"
---

{{< edit_this_page >}}

##Varför

JSON (JavaScript Object Notation) är en vanligt förekommande format för att utbyta data mellan olika applikationer och system. Genom att lära dig att arbeta med JSON kan du enkelt konvertera data mellan olika format och integrera olika tekniker i ditt programmeringsarbete.

##Hur man gör

För att arbeta med JSON i Python, måste du först importera det inbyggda `json` biblioteket. Detta bibliotek ger funktioner för att läsa och skriva JSON-data.

För att läsa en JSON-fil i Python, kan du använda `load()` funktionen. Detta tar in en fil och returnerar datan i ett Python-objekt. Till exempel:

```python
import json

with open('test.json', 'r') as f:
    data = json.load(f) # 'data' är nu ett Python-objekt
```

Du kan också använda `loads()` funktionen för att läsa JSON-data från en sträng istället för en fil. Till exempel:

```python
import json

data_str = '{"name": "Lisa", "age": 25}'
data = json.loads(data_str) # 'data' är nu ett Python-objekt
```

För att skriva JSON-data från ett Python-objekt till en fil, använder du `dump()` funktionen. Detta tar in ett Python-objekt och en fil och skriver den utsträckta datan till filen. Till exempel:

```python
import json

data = {'name': 'Lisa', 'age': 25}

with open('output.json', 'w') as f:
    json.dump(data, f)
```

Du kan också använda `dumps()` funktionen för att skriva JSON-data till en sträng istället för en fil. Till exempel:

```python
import json

data = {'name': 'Lisa', 'age': 25}

data_str = json.dumps(data) # 'data_str' är nu en JSON-sträng
```

##Mer djupdykning

JSON-data består av olika datatyper, som strängar, nummer, listor och dictionaries. När du läser in data med `json.load()` eller `json.loads()`, omvandlar Python automatiskt dessa datatyper till lämpliga Python-objekt. Till exempel, om du har en sträng av ett nummer i din JSON-data, kommer den att omvandlas till en `int` i ditt Python-objekt.

När du skriver ut JSON-data med `json.dump()` eller `json.dumps()`, måste du se till att datan är kompatibel med JSON-formatet. Till exempel måste alla citat tecken vara dubbla citat istället för enkla citat, och alla nycklar i en dictionary måste vara strängar.

Du kan också använda `json.dumps()` med en parameter `indent` för att skapa en mer läsbar JSON-sträng genom att lägga till ett visst antal indrag. Till exempel:

```python
import json

data = {'name': 'Lisa', 'age': 25}

data_str = json.dumps(data, indent=4) # 'data_str' är nu en läsbar JSON-sträng med 4 indrag
```

##Se även

- [Officiell dokumentation för json-modulen i Python (på engelska)](https://docs.python.org/3/library/json.html)
- [En grundläggande introduktion till JSON (på svenska)](https://www.webbriktlinjer.se/grundlaeggande-json/)
- [JSON formatter för att göra JSON-data mer läsbar (på engelska)](https://jsonformatter.org/)