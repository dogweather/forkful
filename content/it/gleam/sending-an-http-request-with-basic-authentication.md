---
title:                "Inviare una richiesta http con autenticazione di base"
date:                  2024-01-20T18:01:38.574640-07:00
model:                 gpt-4-1106-preview
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa & Perché?)
Inviare una richiesta HTTP con autenticazione di base significa aggiungere le credenziali di un utente (nome utente e password) nell'intestazione della richiesta. I programmatori lo fanno per accedere a risorse protette su un server.

## How to: (Come fare:)
In Gleam, useremo il pacchetto `http` per gestire le nostre richieste. Ecco un esempio:

```gleam
import gleam/http
import gleam/http.{BasicAuth, Request}

fn send_authenticated_request() -> http.Result {
  let auth = BasicAuth(
    username: "user",
    password: "pass",
  )
  let request = Request(
    method: http.Get,
    url: "https://api.esempio.com/dati",
    body: http.BodyNone,
    headers: [],
    basic_auth: Some(auth),
  )

  http.send(request)
}
```

Se funziona correttamente, avrai una risposta dal server con i dati richiesti.

## Deep Dive (Approfondimento)
L'autenticazione di base HTTP è un metodo vecchio quanto HTTP stesso. Inserisci semplicemente le credenziali codificate in Base64 nell'header `Authorization`. Nonostante la semplicità, ricorda che senza HTTPS, questo metodo è insicuro perché le credenziali possono essere facilmente intercettate.

Alternative? OAuth e i token JWT (JSON Web Token) offrono maggior sicurezza. Scegli basandoti sulle esigenze di sicurezza e sul tipo di API con cui interagisci.

Dettagli implementativi? In Gleam, il tipo `BasicAuth` gestisce i dettagli dell'header dell'autenticazione. La funzione `http.send()` si occupa di eseguire la richiesta.

## See Also (Vedi Anche)
- Documentazione Gleam su HTTP: https://gleam.run/std-lib/gleam/http/
- RFC per l'Autenticazione HTTP Base: https://tools.ietf.org/html/rfc7617
- Tutorial su OAuth: https://oauth.net/getting-started/ 
- JWT.io, risorse sui JSON Web Tokens: https://jwt.io/introduction/