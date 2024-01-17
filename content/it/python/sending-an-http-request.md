---
title:                "Inviare una richiesta http"
html_title:           "Python: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

#Ciao amici Pythonisti!
Se siete programmatori, sicuramente avete sentito parlare di "invio di richieste HTTP". Ma cosa significa esattamente e perché i programmatori lo fanno?

## Cosa & Perché?
In breve, inviare una richiesta HTTP significa comunicare con un server web per ottenere informazioni o eseguire un'azione. I programmatori lo fanno per creare applicazioni web dinamiche, ad esempio per l'elaborazione dei dati o per l'accesso ai servizi di terze parti.

## Come fare:
Ecco un esempio di codice in Python per inviare una richiesta HTTP utilizzando il modulo "requests" e ottenere il codice di risposta e il contenuto della pagina:

```Python
import requests

response = requests.get("https://www.google.com/")
print(response.status_code)
print(response.text)
```
Ecco il risultato: 
```
200
<!doctype html><html itemscope="" itemtype="http://schema.org/WebPage" lang="it"><head><meta content="/images/branding/googleg/1x/googleg_standard_color_128dp.png" itemprop="image"><title>Google</title>...
```

## Approfondiamo:
In passato, i programmatori dovevano scrivere manualmente le richieste HTTP utilizzando il protocollo TCP/IP. Oggi, possono utilizzare librerie come "requests" che semplificano notevolmente il processo. Altre alternative per inviare richieste HTTP includono la libreria urllib e il modulo HTTP di Python standard. Nel dettaglio, una richiesta HTTP è costituita da un metodo (GET, POST, PUT, ecc.), un URL e un insieme di intestazioni e parametri opzionali per personalizzare la richiesta.

## Vedi anche:
- Il tutorial "Requests: HTTP per Esseri Umani" su Python.org
- La documentazione ufficiale del modulo "requests"
- Il tutorial su richieste HTTP con Python su Real Python