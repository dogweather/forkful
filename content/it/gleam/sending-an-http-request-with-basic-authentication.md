---
title:                "Gleam: Invio di una richiesta http con autenticazione di base"
simple_title:         "Invio di una richiesta http con autenticazione di base"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Perché

L'invio di una richiesta HTTP con autenticazione di base è un'operazione essenziale per l'interazione tra un'applicazione e un server. Questo tipo di autenticazione è comunemente utilizzato per garantire l'accesso sicuro a risorse web protette.

## Come fare

Per inviare una richiesta HTTP con autenticazione di base in Gleam, è necessario utilizzare un modulo esterno come "gleam/http" e seguire alcuni semplici passi. Di seguito è riportato un esempio di codice che mostra come inviare una richiesta GET con autenticazione di base:

```Gleam
import gleam/http
import gleam/option
import gleam/io
 
let url = "https://www.example.com/api"
let username = "myusername"
let password = "mypassword"
 
let request =
  http
  |> http.basic_auth(username, password)
  |> http.get(url)

let response = request
  |> http.send
 
response
  |> option.unwrap_or_else(ignore)
  |> io.println
```

L'output di questo codice sarà una risposta JSON dal server, che può essere utilizzata per ulteriori operazioni.

## Approfondimento

In profondità, l'autenticazione di base si basa sull'utilizzo di una stringa di credenziali codificata in base64. Questa stringa è costituita dal nome utente e dalla password separati da un due punti e poi codificati in base64. Questa stringa viene quindi inviata nel campo "Authorization" dell'intestazione HTTP della richiesta. Il server verifica quindi le credenziali inviate e concede l'accesso se sono corrette.

## Vedi anche

- [Documentazione sul modulo http di Gleam](https://gleam.run/modules/http/)
- [Esempio di autenticazione di base in Gleam](https://github.com/gleam-lang/gleam/blob/master/examples/http_basic_auth.gleam)