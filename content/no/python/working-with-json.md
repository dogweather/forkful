---
title:                "Arbeid med JSON"
date:                  2024-01-19
html_title:           "Arduino: Arbeid med JSON"
simple_title:         "Arbeid med JSON"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON er dataformatet for webben. Programmerere bruker det for å lagre og overføre kompakt og leselig data mellom server og klient.

## How to:

```python
import json

# Lage JSON fra en Python dict
data = {'navn': 'Ola', 'age': 30}
json_data = json.dumps(data)
print(json_data)

# Parse JSON tilbake til Python dict
parsed_data = json.loads(json_data)
print(parsed_data)
```

Sample output:
```
{"navn": "Ola", "age": 30}
{'navn': 'Ola', 'age': 30}
```

```python
# Les JSON fra fil
with open('data.json', 'r', encoding='utf-8') as f:
    data_fra_fil = json.load(f)
    print(data_fra_fil)
```

## Deep Dive
JSON står for JavaScript Object Notation og ble introdusert tidlig på 2000-tallet. Alternativer inkluderer XML og YAML, men JSON er populært for enkeltheten og hastigheten. Når du jobber med Python, skjer serialisering og deserialisering av JSON via `json` modulen, som følger RFC7159 standarden.

## See Also
- [JSON i Python dokumentasjon](https://docs.python.org/3/library/json.html)
- [RFC 7159 – The JavaScript Object Notation (JSON) Data Interchange Format](https://tools.ietf.org/html/rfc7159)
- [W3Schools JSON tutorial](https://www.w3schools.com/js/js_json_intro.asp)
