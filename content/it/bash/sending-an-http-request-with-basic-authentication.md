---
title:                "Inviare una richiesta http con autenticazione di base"
html_title:           "Bash: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?
 

In Bash, inviare una richiesta HTTP con autenticazione di base significa inviare una richiesta HTTP con un token di accesso nell'intestazione di autorizzazione. I programmatori lo fanno per accedere in modo sicuro a risorse protette da autenticazione, come API o pagine web.

## Come Fare:

```Bash
# Esempio di invio di una richiesta HTTP con autenticazione di base utilizzando cURL
curl --user username:password https://example.com/api/endpoint
```

```Bash
# Output di una richiesta HTTP con autenticazione di base
{
   "data":{
      "id": 123,
      "username": "user123",
      "email": "user123@example.com"
   }
   "message": "Autenticazione riuscita"
}
```

## Approfondimento:

1. Contesto storico: L'autenticazione di base è uno dei primi metodi di autenticazione utilizzati nelle applicazioni web. È stato sostituito da metodi più sicuri, ma è ancora ampiamente usato per la sua semplicità.

2. Altre alternative: Oltre all'autenticazione di base, esistono altri metodi di autenticazione come OAuth o JWT, che offrono maggiori livelli di sicurezza e flessibilità.

3. Dettagli di implementazione: L'autenticazione di base richiede l'utilizzo di un nome utente e una password, che vengono codificati in base64 e aggiunti all'intestazione di autorizzazione della richiesta HTTP.

## Vedi anche:

- [Guida completa all'autenticazione HTTP](https://blog.restcase.com/restful-api-authentication-basics/)
- [Documentazione cURL](https://curl.haxx.se/docs/)