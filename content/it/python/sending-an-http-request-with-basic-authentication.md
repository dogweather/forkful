---
title:                "Inviare una richiesta http con autenticazione di base"
html_title:           "Bash: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Che Cosa & Perché?
L'invio di una richiesta HTTP con autenticazione di base è un metodo sicuro per accedere a risorse protette sul Web. I programmatori lo usano per autorizzare le loro richieste a servizi web, API ed endpoint che richiedono una forma di autenticazione.

## Come fare:
Ecco un esempio del codice Python per l'invio di una richiesta HTTP con autenticazione di base.

```Python
import requests
from requests.auth import HTTPBasicAuth

url = 'https://your-url.com/endpoint'

auth_values = ('username', 'password')
response = requests.get(url, auth=HTTPBasicAuth(*auth_values))

print(response.status_code)
```
In questo esempio, la `status_code` stampata mostrerà l'esito della nostra richiesta - `200` per il successo, `401` per credenziali non corrette, ecc.

## Nel Dettaglio
L'autenticazione HTTP di base, una tecnica introdotta nel 1996 come parte della specifica HTTP 1.0, implica l'invio di credenziali utente in una stringa codificata in base64 all'interno di un header HTTP.

Tuttavia, ci sono alternative più sicure come l'autenticazione Digest o l'autenticazione token-based come OAuth. La scelta attiene alle esigenze del progetto e del livello di sicurezza necessario.

In termini di dettagli di implementazione, `requests.get` fa una richiesta GET al server. L'argomento `auth` accetta un oggetto auth di tipo tuple, che viene poi convertito in una stringa codificata in base64 dal modulo `HTTPBasicAuth`.

## Vedere Anche
Per ulteriori dettagli, consultate i seguenti link:
- [Requests library documentation](https://docs.python-requests.org/en/latest/)
- [Autenticazione HTTP di base su Wikipedia](https://it.wikipedia.org/wiki/Basic_access_authentication)
- [HTTP status codes](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status)