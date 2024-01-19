---
title:                "Inviare una richiesta http con autenticazione di base"
html_title:           "Bash: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
L'invio di una richiesta HTTP con autenticazione di base prevede l’invio di username e password codificati in base64 contenuti nell’header della richiesta HTTP. Questo metodo è utilizzato da programmatori per proteggere l'accesso alle risorse web.

## Come Fare:
Ecco un esempio di codice in Elm per inviare una richiesta HTTP con autenticazione di base:

```Elm
import Http
import Http.Headers as Headers

username = "username"
password = "password"
url = "http://example.com"

auth = "Basic " ++ (Base64.encode (username ++ ":" ++ password))

request =
    Http.get
        { url = url
        , headers = [ Headers.authorization auth ]
        }

Http.send HandleResponse request
```

Dopo l'esecuzione del codice di cui sopra, la richiesta invierà un `authorization` header con valore `Basic <codice_base64>`.

## Approfondimenti
L’autenticazione di base HTTP è un metodo standard di invio di credenziali utente. Tuttavia, è nato quando la sicurezza informatica non era una preoccupazione primaria come lo è oggi. Non codifica le credenziali, ma le codifica solo in codice base64, il che significa che qualcuno con accesso alla rete potrebbe decifrare facilmente username e password.

Come alternativa, potrebbe essere utilizzata l’autenticazione a token, l'autenticazione digest o l'autenticazione OAuth, in particolare nelle applicazioni moderne, per aumentare la sicurezza.

Una cosa da notare su questo esempio è che stiamo utilizzando il modulo Http.Headers per impostare l'header della richiesta. Questo header "Authorization" è un header standard HTTP e il valore "Basic <codice_base64>" è solo uno dei pochi metodi di autenticazione che un server può scegliere di supportare.

## Guardate Anche
- Documentazione sul modulo Http di Elm [qui](https://package.elm-lang.org/packages/elm/http/latest/)
- Informazioni sulla codifica Base64 [qui](https://developer.mozilla.org/it/docs/Glossario/Base64) 
- La documentazione ufficiale sulle richieste HTTP con autenticazione di base [qui](https://tools.ietf.org/html/rfc7617).