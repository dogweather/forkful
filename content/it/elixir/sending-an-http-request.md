---
title:                "Inviare una richiesta http"
html_title:           "C++: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Inviare una richiesta HTTP significa chiedere dati a un server. I programmatori lo fanno per interagire e recuperare informazioni da server e APIs.

## Come fare:

Ecco un esempio di come inviare una richiesta HTTP usando Elixir, con l'aiuto della libreria HTTPoison.

```elixir
HTTPoison.start

{:ok, response} = HTTPoison.get("http://api.esempio.com/dati")

IO.puts(response.body)
```

Il codice sopra invia una richiesta GET al server e poi stampa la risposta.

```elixir
{:ok, response} = HTTPoison.post("http://api.esempio.com/dati", "{\"chiave\": \"valore\"}", [{"Content-Type", "application/json"}])

IO.puts(response.status_code)
```

Questo invece invia una richiesta POST e poi stampa lo status della risposta.

## Approfondimento:

Inviare richieste HTTP è un processo antico quanto il web. È il primo passo per costruire applicazioni web interattive. Ce ne sono tante di alternative in Elixir, come Tesla o Finch, ma HTTPoison è uno dei più usati per la sua semplicità. Quando invii una richiesta HTTP, il tuo programma si connette a un server, invia la richiesta e attende la risposta. Questa risposta può essere qualsiasi cosa il server scelga di inviare, come HTML, JSON, o un messaggio di errore.

## Vedi Anche:

- Documentation for HTTPoison: [https://hexdocs.pm/httpoison/readme.html](https://hexdocs.pm/httpoison/readme.html)
- Elixir School: [https://elixirschool.com/](https://elixirschool.com/)
- Official Elixir Site: [https://elixir-lang.org/](https://elixir-lang.org/)