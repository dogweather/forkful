---
title:                "Inviare una richiesta http con autenticazione di base."
html_title:           "Bash: Inviare una richiesta http con autenticazione di base."
simple_title:         "Inviare una richiesta http con autenticazione di base."
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Perché

Sarete felici di sapere che inviare una richiesta HTTP con autenticazione di base usando Bash è più facile di quanto pensiate! In un mondo sempre più digitale, questa è una delle abilità più utili da avere per automatizzare alcune delle tue attività online.

## Come fare

Prima di tutto, dovete avere chiaro che cosa sia l'autenticazione di base. In parole molto semplici, è un modo per proteggere le vostre informazioni sensibili durante il trasferimento di dati su internet. Per inviare una richiesta HTTP con autenticazione di base, dovrete utilizzare il comando `curl` di Bash, che è uno strumento estremamente potente per interagire con le risorse web. Vediamo un esempio pratico:

```
curl -u username:password https://www.example.com
```
In questo comando, stiamo utilizzando l'opzione `u` per specificare le credenziali di accesso tramite il formato `username:password`. Se il server richiede l'autenticazione di base, la richiesta verrà completata con successo e verrà visualizzata la risposta del server.

## Approfondimento

Ora che sappiamo come inviare una richiesta HTTP con autenticazione di base, è importante anche capire come funziona questo processo di autenticazione. Quando il server riceve la richiesta, controlla le credenziali fornite dalla nostra opzione `-u` e se sono corrette, concedere l'accesso alla risorsa richiesta. Questo processo avviene tramite un header speciale chiamato `Authorization`, che contiene il tipo di autenticazione e le credenziali codificate in base64.

## Vedi anche

A questo punto, avete una buona comprensione di come utilizzare l'autenticazione di base in una richiesta HTTP con Bash. Se siete interessati a saperne di più su questo argomento, ecco alcuni link utili che possono aiutarvi:

- [Documentazione ufficiale di cURL](https://curl.haxx.se/docs/manpage.html)
- [Autenticazione di base su Wikipedia](https://it.wikipedia.org/wiki/Autenticazione_base)
- [Come funziona l'autenticazione di base](https://www.freecodecamp.org/news/understanding-basic-authentication-and-how-it-works-17da7633730)