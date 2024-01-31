---
title:                "Arbeta med JSON"
date:                  2024-01-19
simple_title:         "Arbeta med JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/working-with-json.md"
---

{{< edit_this_page >}}

## Vad & Varför?
JSON (JavaScript Object Notation) hanterar data som text i nyckel-värdepar, liknande Python-dictionary. Programmerare använder JSON för att enkelt utbyta data mellan olika språk och tjänster, tack vare dess enkelhet och universalitet.

## How to:
### Läs in JSON
```Python
import json

# Förutsatt att du har en JSON-fil som heter 'data.json'.
with open('data.json', 'r') as f:
    data = json.load(f)
print(data)
```

### Skriv ut JSON
```Python
with open('data.json', 'w') as f:
    json.dump(data, f)
```

### Konvertera sträng till JSON
```Python
json_str = '{"namn": "Anna", "ålder": 30}'
person = json.loads(json_str)
print(person)
```
Output: `{'namn': 'Anna', 'ålder': 30}`

### Konvertera JSON till sträng
```Python
person_str = json.dumps(person)
print(person_str)
```
Output: `'{"namn": "Anna", "ålder": 30}'`

## Deep Dive
JSON introducerades 2001, designad för människoläsbarhet och maskinskrivning. XML är ett alternativ, men JSON är ofta snabbare och mer kompakt. JSON används i REST APIs och webbapplikationer för att skicka data mellan klient och server.

## See Also
- Läs mer om JSON-modulen i Python: https://docs.python.org/3/library/json.html
- Jämför JSON och XML: https://www.json.org/xml.html
- Upptäck JSON Schema för att validera JSON-data: http://json-schema.org/
