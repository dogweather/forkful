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

## Perché

Probabilmente ti sarai chiesto almeno una volta: perché dovrei inviare una richiesta HTTP con l'autenticazione di base? La risposta è semplice, con l'autenticazione di base puoi garantire l'accesso sicuro ai tuoi dati e servizi su una rete.

## Come usare l'autenticazione di base in Python

Per utilizzare l'autenticazione di base in Python, devi seguire questi semplici passaggi:

1. Importa il modulo "requests" per effettuare richieste HTTP.
2. Definisci le credenziali dell'utente (nome utente e password) da utilizzare per l'autenticazione.
3. Passa le credenziali all'intestazione dell'autenticazione di base nella richiesta HTTP.
4. Fai la richiesta al server desiderato.

Di seguito un esempio di codice che effettua una richiesta GET con autenticazione di base e stampa l'output nella console:

```Python
import requests

username = "mario"
password = "p@ssw0rd"

response = requests.get("https://api.example.com", auth=(username, password))

print(response.text)
```

L'output nella console sarà il contenuto della risposta ricevuta dal server.

## Approfondimento

L'autenticazione di base è uno dei metodi di autenticazione più semplici e ampiamente utilizzati per garantire la sicurezza delle comunicazioni su una rete. Consiste nell'invio di un nome utente e una password in chiaro all'interno delle intestazioni della richiesta HTTP.

Per garantire una maggiore sicurezza, è consigliato utilizzare il protocollo HTTPS, che cifra i dati scambiati tra il client e il server. Inoltre, è importante utilizzare password sicure e non condividerle con nessuno.

## Vedi anche

- [Documentazione ufficiale di Python](https://docs.python.org/3/library/urllib.request.html?highlight=authentication#urllib.request.Request.add_header)
- [Tutorial su come utilizzare l'autenticazione di base in Python](https://www.geeksforgeeks.org/send-getpost-request-using-python/)
- [Manuale di riferimento di requests](https://requests.readthedocs.io/en/master/user/authentication/)