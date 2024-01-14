---
title:                "Python: Inviare una richiesta HTTP con autenticazione di base"
simple_title:         "Inviare una richiesta HTTP con autenticazione di base"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Perché

Perché dovresti impegnarti nella creazione di una richiesta HTTP con autenticazione di base? In un mondo sempre più interconnesso, l'utilizzo di autenticazione di base è una pratica comune per proteggere l'accesso ai dati sensibili sui siti web. Imparare a inviare richieste HTTP con autenticazione di base ti aiuterà ad acquisire una conoscenza fondamentale degli strumenti di sicurezza del web e potrebbe essere utile per il tuo lavoro o il tuo prossimo progetto.

## Come fare

Per inviare una richiesta HTTP con autenticazione di base in Python, segui i seguenti passaggi:

1. Importa il modulo `requests` per gestire le richieste HTTP:

```Python
import requests
```

2. Crea un dizionario contenente le credenziali di autenticazione di base, sostituendo `username` e `password` con le tue credenziali:

```Python
auth = {'username': 'password'}
```

3. Utilizza il metodo `get` di `requests` per inviare la richiesta al server desiderato, passando come parametro il dizionario delle credenziali di autenticazione:

```Python
response = requests.get('https://www.example.com', auth=auth)
```

4. Controlla lo stato della risposta utilizzando il metodo `status_code` e stampa il contenuto della risposta utilizzando il metodo `text`:

```Python
print(response.status_code)
print(response.text)
```

## Approfondimento

In una richiesta con autenticazione di base, il client invia le credenziali come parte dell'header della richiesta, utilizzando un codice di autenticazione codificato Base64. Il server verifica quindi le credenziali fornite per concedere l'accesso o rifiutare la richiesta. È importante notare che l'utilizzo di autenticazione di base non è considerato completamente sicuro, poiché le credenziali sono facilmente decodificabili. Pertanto, è consigliabile utilizzare un metodo di autenticazione più sicuro, come HTTPS, per proteggere le informazioni sensibili.

## Vedi anche

- [Documentazione ufficiale di Requests su autenticazione di base in Python](https://docs.python-requests.org/en/master/user/authentication/)
- [Tutorial su autenticazione di base in Python su Real Python](https://realpython.com/intermediate-python-requests/)
- [Informazioni sull'utilizzo di autenticazione di base e altre tecniche di sicurezza per proteggere le tue applicazioni web](https://www.owasp.org/index.php/Web_Application_Security_Testing_Cheat_Sheet#Authentication_Testing)