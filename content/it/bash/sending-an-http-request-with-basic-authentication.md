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

# Invio di una richiesta HTTP con autenticazione di base in Bash

### Cos'è e Perché?

L'invio di una richiesta HTTP con autenticazione di base è un processo per condividere dati tra il tuo script Bash e un server web. I programmatori lo fanno per accedere o inviare dati sicuri ai server tramite uno script di Bash.

### Come fare:

Per inviare una richiesta HTTP con autenticazione di base, utilizziamo `curl`. Ecco un esempio di comando:

```Bash
username="utente"
password="password"

curl -u $username:$password http://esempio.com
```

Questo comando invia una richiesta GET al sito `http://esempio.com` utilizzando le credenziali fornite. 

Risposta del server:

```Bash
{"message": "Ciao, utente!"}
```

Ecco un esempio di invio di una richiesta POST con dati:

```Bash
username="utente"
password="password"
data='{"key":"value"}'

curl -u $username:$password -d $data -H "Content-Type: application/json" http://esempio.com
```

### Approfondimenti:

L'autenticazione di base è una delle tecniche più antiche per l'autenticazione su HTTP, introdotta con HTTP/1.0 nel 1996. Nonostante la sua età, è ancora comunemente utilizzata, specialmente per l'interazione con le API REST.

Tuttavia, ci sono alternative. Ad esempio, l'autenticazione con token Bearer viene spesso utilizzata nelle API moderne. Per utilizzare l'autenticazione Bearer, puoi modificare il comando come segue:

```Bash
token="il-tuo-token"

curl -H "Authorization: Bearer $token" http://esempio.com
```

Ricorda che l'autenticazione di base invia username e password come una stringa codificata in base64. Non è cifrata, quindi dovrebbero essere utilizzate connessioni protette come HTTPS.

### Vedi anche:

- Manuali `curl`: [https://curl.haxx.se/docs/manpage.html](https://curl.haxx.se/docs/manpage.html)
- Introduzione alle API RESTful: [https://restfulapi.net/](https://restfulapi.net/)
- HTTP Basic Authentication su MDN: [https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#basic_authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#basic_authentication)