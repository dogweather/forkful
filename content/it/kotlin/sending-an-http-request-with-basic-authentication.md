---
title:                "Inviare una richiesta http con autenticazione di base"
html_title:           "Kotlin: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Cosa & perché?
In poche parole, l'invio di una richiesta HTTP con autenticazione di base implica l'inserimento delle credenziali di accesso (nome utente e password) nel codice per accedere a risorse protette su un server. I programmatori lo fanno per garantire la sicurezza delle loro applicazioni e dei dati sensibili che esse possono accedere.

## Come fare:
Eccoti un esempio di come inviare una richiesta HTTP con autenticazione di base utilizzando Kotlin. Nota che è necessario utilizzare un'istruzione di posta in arrivo e specificare l'URL a cui è indirizzata la richiesta, prima di codificare le credenziali di accesso nella richiesta.

```Kotlin
val username = "username"
val password = "password"

HttpResponse response = Unirest.get("https://www.example.com")
.basicAuth(username, password)
.asEmpty()
```

L'output dovrebbe essere una risposta dello stato "200 OK", indicando che la richiesta è stata inviata con successo e l'accesso è stato autorizzato.

## Approfondimento:
L'autenticazione di base è stato il metodo originale per autenticare le richieste HTTP, introdotto nel 1999. Oggi ci sono metodi più sicuri come l'autenticazione OAuth, ma la semplicità e la compatibilità con vecchi sistemi rendono ancora utile l'utilizzo di questo metodo.

Ci sono diverse alternative all'autenticazione di base, come l'autenticazione basata su token o l'autenticazione a chiave pubblica. Tuttavia, l'utilizzo di questi metodi può essere più complesso e richiedere una configurazione aggiuntiva nel server.

Per implementare correttamente l'autenticazione di base su un server, è necessario utilizzare un sistema di hashing per crittografare la password e preventivamente criptarla prima di inviarla nella richiesta.

## Vedi anche:
- [Autenticazione di base HTTP](https://developer.mozilla.org/it/docs/Web/HTTP/Authentication)
- [Introduzione alle richieste HTTP con Kotlin](https://kotlinlang.org/docs/reference/http-client.html)
- [Sicurezza nelle applicazioni Kotlin](https://kotlinlang.org/docs/reference/coroners.html)