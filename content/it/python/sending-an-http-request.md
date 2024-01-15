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

## Perché

Se sei interessato a programmazione o sei un programmatore esperto, probabilmente hai già sentito il termine "HTTP request". In questo articolo, scoprirai perché le richieste HTTP sono fondamentali per l'interazione tra client e server in un'applicazione web. 

## Come fare

Per creare una richiesta HTTP in Python, segui questi semplici passi:

1. Importa il modulo `requests` nella tua applicazione: 
```Python
import requests
```

2. Crea un oggetto `request` specificando l'URL del server e il metodo HTTP desiderato, ad esempio `GET`, `POST`, `PUT` o `DELETE`:
```Python
r = requests.get('https://www.example.com')
```

3. Opzionalmente, puoi aggiungere dei parametri o dei dati alla tua richiesta, ad esempio se il server richiede un token di autenticazione:
```Python
payload = {'token': 'abcd123'}
r = requests.get('https://www.example.com', params=payload)
```

4. Invia la richiesta al server e salva la risposta in un oggetto `response`:
```Python
response = r.text
```

5. Puoi quindi utilizzare i metodi e gli attributi dell'oggetto `response` per ottenere informazioni sulla risposta dal server, come ad esempio lo stato della richiesta (`response.status_code`), l'header (`response.headers`), o il contenuto della risposta (`response.text`).

## Approfondimento

Le richieste HTTP sono un modo standard per comunicare tra client e server in un'applicazione web. Utilizzando il protocollo HTTP, un client può inviare richieste al server e ottenere le risposte corrispondenti. Le richieste HTTP sono composte da un URL che identifica il server e il metodo HTTP, come ad esempio `GET`, `POST`, `PUT` o `DELETE`, che specifica l'azione da eseguire sul server.

Il modulo `requests` è uno strumento potente e versatile per inviare richieste HTTP in Python. Oltre all'esempio di base mostrato sopra, offre anche molte altre funzionalità avanzate, come ad esempio l'utilizzo di cookie, gestione di autenticazione, e la possibilità di inviare dati in formato JSON. Ti consigliamo di esplorare la documentazione ufficiale di `requests` per saperne di più.

## Vedi anche

- [Documentazione ufficiale di `requests`](https://requests.readthedocs.io/en/master/)
- [Guida alla programmazione di HTTP in Python](https://realpython.com/python-requests/)
- [Che cos'è HTTP e come funziona](https://www.w3schools.com/whatis/whatis_http.asp)