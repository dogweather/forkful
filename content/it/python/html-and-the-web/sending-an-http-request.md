---
title:                "Inviare una richiesta http"
date:                  2024-01-20T18:00:20.173494-07:00
model:                 gpt-4-1106-preview
simple_title:         "Inviare una richiesta http"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa e Perché?)
Inviare una richiesta HTTP significa chiedere o inviare dati a un server web. I programmatori lo fanno per interagire con API, scaricare contenuti o inviare informazioni.

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