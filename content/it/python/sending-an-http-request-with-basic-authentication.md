---
title:                "Invio di una richiesta http con autenticazione di base"
html_title:           "Python: Invio di una richiesta http con autenticazione di base"
simple_title:         "Invio di una richiesta http con autenticazione di base"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Invio di una richiesta HTTP con autenticazione di base è una pratica comune tra i programmatori per accedere a risorse protette su una rete o un server. Questo tipo di autenticazione richiede la fornitura di credenziali login, generalmente un nome utente e una password, per verificare l'identità dell'utente.

## Come fare:
Ecco un esempio di codice che mostra come inviare una richiesta HTTP con autenticazione di base utilizzando il modulo `requests` in Python 3:

```python
import requests
from requests.auth import HTTPBasicAuth

# Specifica le credenziali login
username = 'username'
password = 'password'

# Crea un'istanza della classe HTTPBasicAuth con le credenziali login
auth = HTTPBasicAuth(username, password)

# Invia una richiesta GET al server protetto con autenticazione di base
r = requests.get('https://example.com/protected', auth=auth)

# Stampa il codice di stato della risposta
print(r.status_code)
```

L'output dovrebbe essere il codice di stato `200` che indica una richiesta di successo.

## Approfondimento:
L'autenticazione di base con richieste HTTP è stato introdotta come parte della specifica HTTP/1.0 e continua ad essere utilizzata come uno dei metodi di autenticazione più semplici e diffusi nelle applicazioni web. Tuttavia, poiché le credenziali vengono inviate in chiaro, questo metodo non è sicuro e si consiglia di utilizzarlo solo in connessioni HTTPS (HTTP over SSL).

Un'alternativa all'autenticazione di base è l'utilizzo di un token di autenticazione, che può essere generato una volta e utilizzato per successive richieste senza la necessità di memorizzare e inviare le credenziali di login ogni volta.

## Vedi anche:
- [Documentazione ufficiale del modulo `requests`](https://requests.readthedocs.io/en/master/)
- [Specifica HTTP/1.0](https://tools.ietf.org/html/rfc1945)