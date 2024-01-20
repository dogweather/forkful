---
title:                "Inviare una richiesta http"
html_title:           "C++: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?
Inviare una richiesta HTTP è un processo per ottenere dati da un server web utilizzando il protocollo HTTP. I programmatori lo fanno per interagire con servizi web, recuperare informazioni, inviare dati, tra gli altri.

## Come fare:
Per inviare una richiesta HTTP in Python, puoi utilizzare il modulo `requests`. Un esempio di base è il seguente:

```Python
import requests

response = requests.get('http://example.com')

print(response.status_code)
print(response.text)
```
Correndo il codice sopra, vedresti qualcosa del genere:

```
200
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
    ...
```

## Approfondimento
Inviare richieste HTTP è una pratica comune nella programmazione dal momento che il web è costruito su questo protocollo. Prima che entrasse in scena Python, si dovevano gestire manualmente le connessioni TCP. Con l'arrivo di Python e moduli come `requests`, è diventato molto più semplice.

Un'alternativa a `requests` in Python è `http.client` che fa parte della libreria standard. Tuttavia, `requests` è molto più facile da usare e offre più funzionalità.

Quando invii una richiesta HTTP, in realtà stai inviando un messaggio testuale a un server web. Il server risponde allo stesso modo. Il corpo di questa risposta viene poi interpretato dal tuo programma.

## Vedi anche
- Documentazione di `requests`: https://docs.python-requests.org/en/latest/
- Tutorial HTTP per principianti: https://www3.ntu.edu.sg/home/ehchua/programming/webprogramming/HTTP_Basics.html
- `http.client` documentazione: https://docs.python.org/3/library/http.client.html