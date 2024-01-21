---
title:                "Inviare una richiesta http con autenticazione di base"
date:                  2024-01-20T18:01:27.758107-07:00
model:                 gpt-4-1106-preview
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?
Inviare una richiesta HTTP con autenticazione di base significa trasmettere username e password per accedere a risorse protette su un server. I programmatori lo fanno per interagire con API che richiedono una dimostrazione di identità.

## How to:
Per inviare una richiesta con autenticazione di base in Fish, puoi utilizzare `curl`. Ecco un esempio: 

```Fish Shell
set user "tuo_username"
set password "tua_password"
set encoded_auth (echo -n "$user:$password" | base64)
curl -H "Authorization: Basic $encoded_auth" https://esempio.com/api/dati
```

Output atteso (sarà diverso a seconda dei dati forniti dall'API):
```
{ "dati_risposta": "Ecco i tuoi dati privati!" }
```

## Deep Dive
L'autenticazione HTTP Basic è uno dei metodi più antichi per controllare l'accesso alle risorse web. È semplice ma non il più sicuro, dato che le credenziali sono codificate in base64, un encoding facilmente decodificabile. Alternativamente, si può utilizzare l'autenticazione Digest o dei token, come OAuth, che sono più sicuri. Nell'esempio di Fish, la Base64 codifica l'username e la password prima di aggiungerli all'header della richiesta HTTP, simulando il processo che un browser farebbe automaticamente quando richiesto.

## See Also
- Documentazione `curl`: https://curl.se/docs/
- Base64 Encoding wiki: https://it.wikipedia.org/wiki/Base64
- Autenticazione HTTP: https://developer.mozilla.org/it/docs/Web/HTTP/Authentication