---
title:                "Inviare una richiesta http con autenticazione di base"
html_title:           "Elixir: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

In poche parole, inviare una richiesta HTTP con autenticazione di base significa includere nell'intestazione dell'HTTP request le credenziali dell'utente, come username e password, per accedere ad un servizio o una risorsa protetta. I programmatori fanno questo per garantire la sicurezza delle loro applicazioni e dei dati utente.

## Come:

```Elixir
identifier = "username" 
password = "password"

request = HTTP.basic_auth(identifier, password) 
response = HTTPc.post("https://www.example.com/", [], [body: "{}"], [request: request]) 
```

Output:

```Elixir
{:ok, %HTTPo.Request{id: ..., method: :post, headers: [..., {"Authorization", "Basic ..."}], ...}}
```

## Approfondimento:

In passato, inviare una richiesta HTTP con autenticazione di base era uno dei metodi più comuni per proteggere le risorse web e garantire l'accesso solo agli utenti autorizzati. Al giorno d'oggi, ci sono alternative più sicure come OAuth o JSON Web Token (JWT), ma inviare una richiesta HTTP con autenticazione di base è ancora una pratica comune e supportata dai linguaggi di programmazione.

## Vedi anche:

Per maggiori informazioni su come inviare una richiesta HTTP con autenticazione di base in Elixir, puoi consultare la documentazione ufficiale di Elixir o cercare su forum e siti di programmazione per ottenere consigli e suggerimenti da altri sviluppatori.