---
date: 2024-01-20 18:00:20.173494-07:00
description: 'How to: (Come Fare:) Usiamo `requests`, una libreria elegante e semplice.
  Installala con `pip install requests`. Ecco un esempio.'
lastmod: '2024-04-05T21:53:43.781858-06:00'
model: gpt-4-1106-preview
summary: (Come Fare:) Usiamo `requests`, una libreria elegante e semplice.
title: Inviare una richiesta http
weight: 44
---

## How to: (Come Fare:)
Usiamo `requests`, una libreria elegante e semplice. Installala con `pip install requests`. Ecco un esempio:

```Python
import requests

# GET request
response = requests.get('https://api.github.com')
print("Status code:", response.status_code)

# POST request con dati JSON
json_data = {'chiave': 'valore'}
response = requests.post('https://httpbin.org/post', json=json_data)
print("Contenuto risposta JSON:", response.json())
```

Output per GET request:
```
Status code: 200
```

Output per POST request:
```
Contenuto risposta JSON: {
  ...
  "json": {
    "chiave": "valore"
  },
  ...
}
```

## Deep Dive (Approfondimento)
Le richieste HTTP sono la base della comunicazione web dal 1991, anno di nascita del protocollo HTTP. Prima di `requests`, Python aveva `urllib`, più ostica. Alternative moderne? `httpx`, supporta HTTP/2. Attenzione ai dettagli: gestisci timeout, errori e sii prudente con i dati sensibili.

## See Also (Vedi Anche)
- Documentazione `requests`: https://requests.readthedocs.io
- Tutorial ufficiale Python su `urllib`: https://docs.python.org/3/library/urllib.html
- `httpx` per HTTP/2: https://www.python-httpx.org
