---
title:                "Inviare una richiesta http con autenticazione di base"
html_title:           "Bash: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Inviare una richiesta HTTP con autenticazione di base è una delle attività di programmazione più comuni. Permette a un client di fornire un nome utente e una password durante l'interazione con un server. Questo è spesso anche usato per le API REST per permettere la sicurezza tra le operazioni del client e del server. 

## Come fare:
Gleam rende relativamente semplice inviare una richiesta HTTP con autenticazione di base. Di seguito è riportato un esempio di come potrebbe essere eseguita tale azione:

```Gleam
import gleam/http.{Request}

let request = Request.new("GET", "https://example.com")
  |> Request.basic_auth("username", "password")

let _ = http.send(request)
```

Nell'esempio, prima viene creata una nuova richiesta HTTP. Poi, con l'uso dell'operatore pipe |> (che passa il risultato della prima funzione come argomento della seconda funzione), l'autenticazione di base viene aggiunta alla richiesta HTTP. Infine, la richiesta viene inviata.

## Approfondimento
Negli anni '90, quando il protocollo HTTP (Hypertext Transfer Protocol) stava guadagnando popolarità, l'autenticazione di base era l'unica modalità disponibile per autenticare le richieste. Nonostante ciò, a causa della mancanza di sicurezza, è stata sostituita da tecnologie più sicure come OAuth o token JWT.

Ora, gli sviluppatori di Gleam possono sfruttare la funzione `basic_auth` per facilitare l'autenticazione di base in una richiesta HTTP. Ma ricorda, l'autenticazione di base non è sicura da sola. Le tue credenziali inviate sono codificate in base64, non criptate. Quindi, dovrebbe essere usata solo su connessioni protette, come HTTPS.

## Leggi Anche
Se sei interessato ad approfondire l'autenticazione HTTP di base, ecco alcuni link utili per approfondire ulteriormente. Da notare che la documentazione ufficiale di Gleam e HTTP forniscono dettagli più tecnici:

1. [Documentazione ufficiale di Gleam](https://gleam.run/)
2. [Specifiche HTTP](https://tools.ietf.org/html/rfc2617)