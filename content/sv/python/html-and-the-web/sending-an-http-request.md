---
title:                "Skicka en http-förfrågan"
aliases: - /sv/python/sending-an-http-request.md
date:                  2024-01-20T18:00:27.207528-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skicka en http-förfrågan"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Skicka en HTTP-begäran är hur din kod pratar med webbservern. Programmerare gör detta för att hämta data, skicka information eller interagera med olika webbtjänster.

## How to:
I Python använder vi ofta `requests`-biblioteket för att hantera HTTP-begäranden.

```Python
import requests

# Get-begäran
response = requests.get('https://api.example.com/data')
print(response.status_code)
print(response.json())

# Post-begäran med data
payload = {'key1': 'value1', 'key2': 'value2'}
response = requests.post('https://api.example.com/submit', json=payload)
print(response.status_code)
print(response.text)
```

Sample Output:
```
200
{'data': ['item1', 'item2', 'item3']}
200
{"result":"success","message":"Data received"}
```

## Deep Dive
HTTP-begäranden modellerar webbkommunikation sedan tidigt 90-tal. `requests` är inte den enda vägen; `http.client` finns i Python's standardbibliotek, men `requests` är enklare. När du skickar en begäran använder `requests` metoderna GET, POST, PUT, DELETE m.fl. för att specificera begäranstypen.

## See Also
- `requests` dokumentation: https://requests.readthedocs.io
- RESTful API-design: https://restfulapi.net/
- HTTP-specifikationer: https://httpwg.org/specs/
- Python `http.client` dokumentation: https://docs.python.org/3/library/http.client.html
