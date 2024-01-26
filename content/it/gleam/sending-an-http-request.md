---
title:                "Inviare una richiesta http"
date:                  2024-01-20T17:59:43.556018-07:00
model:                 gpt-4-1106-preview
simple_title:         "Inviare una richiesta http"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?
Mandare una richiesta HTTP, in pratica, è come bussare alla porta di un sito web per chiedere informazioni o per inviarne. I programmatori lo fanno per interagire con servizi web, per esempio per scaricare dati, inviare form, o comunicare con API.

## How to:
Installiamo `gleam_http` per iniziare. Aggiungi al tuo `rebar.config`:

```erlang
{deps, [
    {gleam_http, "0.8.1"}
]}.
```

Poi, un esempio di codice per mandare una GET request:

```gleam
import gleam/http
import gleam/httpc

pub fn send_get_request() {
  case httpc.send(http.Request(method: Get, url: "https://api.example.com/data", headers: [])) {
    Ok(response) ->
      io.println(response.body)
    Error(error) ->
      io.println("Oops, something went wrong: " ++ error)
  }
}
```
Esegui e guarda la magia.

## Deep Dive
Inviare richieste HTTP è essenziale fin dall'inizio del web. Prima facevamo tutto in CGI e Perl, ora abbiamo un sacco di lingue e pacchetti per farlo, tipo `curl` in Linux, `NSURLRequest` in Swift, o `HttpClient` in .NET.

Usando Gleam, ci si allinea alla visione di Erlang/BEAM per la concorrenza e tolleranza agli errori. Non solo mandi una richiesta HTTP - lo fai in modo robusto, gestendo fallimenti e scalando facilmente.

Le alternative in Gleam comprendono l'uso di librerie esterne come `gun` o `hackney`, ma `gleam_http` e `httpc` sono spesso tutto ciò di cui hai bisogno.

## See Also
- Documentazione per `gleam_http`: https://hexdocs.pm/gleam_http/
- Repo GitHub di Gleam HTTP: https://github.com/gleam-lang/http
- Guida BEAM per la concorrenza: https://erlang.org/doc/design_principles/concurrency.html
- Esempi di richieste HTTP in altre lingue per confronto: https://www.codecademy.com/articles/http-requests
