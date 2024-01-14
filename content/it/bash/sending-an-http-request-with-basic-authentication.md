---
title:                "Bash: Invio di una richiesta http con autenticazione di base"
simple_title:         "Invio di una richiesta http con autenticazione di base"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Perché

L'invio di una richiesta HTTP con autenticazione di base può essere utile per accedere a risorse protette su un server. Questo metodo di autenticazione richiede l'utilizzo di un nome utente e una password per accedere alle risorse.

## Come fare

Per inviare una richiesta HTTP con autenticazione di base, è necessario utilizzare il comando "curl" in Bash. Di seguito è riportato un esempio di codice Bash che mostra come utilizzare "curl" per inviare una richiesta HTTP con autenticazione di base:

```Bash
curl -u nome_utente:password https://www.example.com
```

Nell'esempio sopra, il nome utente e la password sono inseriti dopo il flag "-u". Notare che il nome utente e la password devono essere separati da due punti ":".

Una volta eseguito il comando, verrà inviata una richiesta HTTP al server specificato e, se l'autenticazione viene accettata, verrà restituita la risorsa protetta.

Di seguito è riportato un esempio di output:

```Bash
HTTP/1.1 200 OK
Content-Type: text/html; charset=UTF-8
Date: Sat, 06 Feb 2021 12:00:00 GMT
Server: Apache
Content-Length: 127

<!DOCTYPE html>
<html>
<head>
  <title>Ciao Mondo!</title>
</head>
<body>
  <h1>Buongiorno!</h1>
</body>
</html>
```

## Approfondimento

Il metodo di autenticazione di base è uno dei più semplici e meno sicuri. Ciò è dovuto al fatto che il nome utente e la password sono inviati in chiaro, ovvero senza la crittografia dei dati. Ciò rende più facile per hacker o cracker avere accesso alle credenziali di accesso e accedere a risorse protette.

Un modo per aumentare la sicurezza è utilizzare una connessione protetta come HTTPS, che utilizza la crittografia dei dati durante la trasmissione. Ciò rende più difficile per terze parti leggere le credenziali di accesso.

Inoltre, è importante utilizzare password sicure, composte da una combinazione di lettere maiuscole e minuscole, numeri e caratteri speciali. Questo rende più difficile per hacker o cracker indovinare la password e accedere alle risorse protette.

## Vedi anche

- Tutorial di Bash: https://www.linux.it/tutorial/linux-segreti/sistemi-bash-security
- Documentazione di curl: https://curl.se/docs/
- Guida all'autenticazione di base: https://developer.mozilla.org/it/docs/Web/HTTP/Authentication