---
title:                "Inviare una richiesta http con autenticazione di base"
html_title:           "Bash: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Cosa e Perché? 
Inviare una richiesta HTTP con autenticazione di base è un metodo che permette l'accesso ad un servizio web collezionando username e password del client. I programmatori lo fanno per garantire la sicurezza, prevenire accessi non autorizzati alle risorse web.

## Come fare: 

Utilizziamo la libreria Fuel di Kotlin per inviare una richiesta HTTP. 

```Kotlin
import com.github.kittinunf.fuel.httpGet

val username = "username"
val password = "password"

val httpAsync = "$YOUR_URL"
    .httpGet()
    .authenticate(username, password)
    .response() { request, response, result -> }
```
Qui, `httpGet()` è il metodo che invia la richiesta HTTP. `authenticate(username, password)` consente l'autenticazione di base.

## Approfondimento:

L'autenticazione di base HTTP è uno dei metodi più antichi utilizzato per fornire controllo di accesso ai servizi web. Tuttavia, poiché le credenziali non sono crittografate, non è l'opzione più sicura. Un'alternativa migliore potrebbe essere l'autenticazione 'Bearer', in cui un token di sicurezza viene inviato come parte dell'intestazione HTTP.

Sull'autenticazione di base, le credenziali dell'utente sono inoltrate con ogni richiesta HTTP, aumentando il rischio di intercettazione non sicura. Quindi, dovrebbe essere utilizzato solo su connessioni sicure come HTTPS.

## Vedi anche:

- Leggi di più sull'[autenticazione di base HTTP](https://developer.mozilla.org/it/docs/Web/HTTP/Authentication)
- Per ulteriori dettagli sulla [libreria Fuel di Kotlin](https://fuel.gitbook.io/documentation/).
Presta attenzione alla [sicurezza delle tue applicazioni web](https://www.owasp.org/index.php/Top_10-2017_Top_10).