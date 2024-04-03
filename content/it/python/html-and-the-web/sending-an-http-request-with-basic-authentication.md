---
date: 2024-01-20 18:02:14.845227-07:00
description: "Inviare una richiesta HTTP con autenticazione di base significa fornire\
  \ username e password per accedere a risorse protette su un server web. I\u2026"
lastmod: '2024-03-13T22:44:43.000485-06:00'
model: gpt-4-1106-preview
summary: Inviare una richiesta HTTP con autenticazione di base significa fornire username
  e password per accedere a risorse protette su un server web.
title: Inviare una richiesta http con autenticazione di base
weight: 45
---

## Come si fa:
Ecco come inviare una richiesta HTTP con autenticazione di base in Python utilizzando la libreria `requests`:

```Python
import requests
from requests.auth import HTTPBasicAuth

# sostituisci 'url' con l'URL effettivo
url = "https://api.esempio.com/dati"

# sostituisci 'utente' e 'password' con le tue credenziali
risposta = requests.get(url, auth=HTTPBasicAuth('utente', 'password'))

# output del risultato della richiesta
print(risposta.status_code)
print(risposta.text)
```

Se tutto funziona, vedrai il codice di stato HTTP seguito dai dati richiesti.

## Approfondimento
L'autenticazione di base HTTP è un meccanismo semplice ma non il più sicuro, introdotto negli albori del web. Usa la codifica Base64, ma non crittografa le credenziali, quindi va bene solo su HTTPS. Alternative più sicure includono l'autenticazione Digest e i token OAuth. In Python, l'uso del modulo `requests` semplifica l'invio di richieste con autenticazione di base, ma si può anche usare il più basso livello `http.client` per un controllo più granulare.

## Vedi anche
Per saperne di più, consulta queste risorse:

- Documentazione `requests`: https://requests.readthedocs.io/en/latest/
- HTTP Basic Auth (RFC 7617): https://tools.ietf.org/html/rfc7617
- Sicurezza dei meccanismi di autenticazione HTTP: https://www.owasp.org/index.php/Basic_Authentication
