---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:32.638040-07:00
description: "Hoe: De externe `requests` library van Python maakt het doen van HTTP-aanroepen\
  \ een fluitje van een cent. Hieronder staat hoe je een eenvoudige GET-\u2026"
lastmod: '2024-03-13T22:44:50.373393-06:00'
model: gpt-4-0125-preview
summary: De externe `requests` library van Python maakt het doen van HTTP-aanroepen
  een fluitje van een cent.
title: Een HTTP-verzoek verzenden
weight: 44
---

## Hoe:
De externe `requests` library van Python maakt het doen van HTTP-aanroepen een fluitje van een cent. Hieronder staat hoe je een eenvoudige GET-aanvraag stuurt:

```python
import requests

response = requests.get('https://api.example.com/data')
print(response.status_code)  # Geeft de statuscode van de reactie weer
print(response.json())      # Als de reactie JSON bevat, drukt het af als een Python dict
```

Meer gedetailleerde POST-aanvraag met JSON payload en aangepaste headers:

```python
import requests
import json

url = "https://api.example.com/submit"
data = {'key': 'value'}
headers = {'Content-Type': 'application/json'}

response = requests.post(url, data=json.dumps(data), headers=headers)

print(response.status_code)
print(response.json())
```

## Diepere Duik
HTTP-verzoeken zijn hoe het web werkt — ze bestaan al sinds de vroege jaren 90. Alternatieven voor Python's `requests` omvatten de standaardbibliotheek `urllib`, maar deze is een beetje omslachtiger.

Het begrijpen van hoe HTTP-verzoeken te versturen, omvat kennis van methoden (GET, POST, PUT, DELETE, enz.), statuscodes (bijv., 200 OK, 404 Niet Gevonden), headers en body data.

Voor streaming of asynchrone verzoeken, zou je de asynchrone tegenhanger van `requests` of het `aiohttp`-pakket kunnen verkennen. Onder de motorkap gebruiken deze bibliotheken Python's `socket` voor ruwe netwerkcommunicatie.

Historisch gezien wordt `requests` beschouwd als de go-to vanwege zijn eenvoud en kracht, maar `httpx`, een nieuwere asynchrone-compatibele bibliotheek, wint terrein.

## Zie Ook
- De documentatie van de `requests`-bibliotheek: https://requests.readthedocs.io
- HTTP-statuscodes uitgelegd: https://developer.mozilla.org/en-US/docs/Web/HTTP/Status
- Python's `urllib` documentatie: https://docs.python.org/3/library/urllib.html
- `httpx`-bibliotheek voor asynchrone HTTP-verzoeken: https://www.python-httpx.org
