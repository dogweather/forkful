---
title:                "Python: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Perché inviare una richiesta HTTP?

In questa guida mostreremo come utilizzare Python per inviare una richiesta HTTP ad un server e ricevere una risposta. Questo può essere utile per accedere ad API, ottenere dati da un sito web, o per scopi di automazione.

## Come inviare una richiesta HTTP in Python

Per inviare una richiesta HTTP in Python, utilizzeremo la libreria "requests". Per prima cosa, dobbiamo importare questa libreria all'inizio del nostro script:

```Python
import requests
```

Successivamente, definiremo la richiesta da inviare utilizzando il metodo "get" della libreria "requests":

```Python
response = requests.get('https://www.example.com')
```

Infine, possiamo accedere alla risposta del server utilizzando l'attributo "text" della variabile "response" e stamparla a schermo:

```Python
print(response.text)
```

L'output dovrebbe essere qualcosa di simile a questo:

```Output
<!DOCTYPE html>
<html>
<head>
  <title>Esempio</title>
</head>
<body>
  <h1>Benvenuto su Example.com!</h1>
</body>
</html>
```

## Approfondimento sull'invio di richieste HTTP

La libreria "requests" offre numerosi metodi e opzioni per la gestione delle richieste HTTP, come l'aggiunta di parametri, header personalizzati e autenticazione. Inoltre, possiamo utilizzare la funzione "post" invece di "get" per inviare richieste con dati nel corpo della richiesta.

Per ulteriori informazioni sull'utilizzo di "requests" e sulle richieste HTTP in generale, consigliamo di consultare la [documentazione ufficiale](https://requests.readthedocs.io/en/master/) o [questo tutorial](https://www.toptal.com/python/an-introduction-to-mocking-in-python) su "HTTP mocking" con Python.

## Vedi anche

- [Documentazione ufficiale di requests](https://requests.readthedocs.io/en/master/)
- [Tutorial su HTTP mocking con Python](https://www.toptal.com/python/an-introduction-to-mocking-in-python)
- [Guida introduttiva a Python](https://www.learnpython.org/it/)