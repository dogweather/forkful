---
title:                "Inviare una richiesta http"
html_title:           "Elixir: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?
In poche parole, l'invio di una richiesta HTTP è il modo in cui i programmatori comunicano con un server tramite il protocollo di trasferimento ipertestuale (HTTP). Questo è fondamentale per l'esecuzione di molte operazioni online, come invio di dati a un server o ottenere informazioni da esso.

## Come fare:
Utilizzando il linguaggio di programmazione Elixir, è possibile inviare richieste HTTP tramite il modulo `HTTPoison`. Ecco un esempio di codice che invia una richiesta GET e stampa il suo output:

```Elixir
response = HTTPoison.get("https://example.com")
IO.puts response.body
```

Questo codice invia una richiesta GET al sito web "example.com" e stampa il suo contenuto del corpo nella console.

## Approfondimento:
L'invio di richieste HTTP è stato introdotto per la prima volta nei primi anni '90 come parte del protocollo HTTP/1.0. Prima di allora, le comunicazioni tra i client e i server erano limitate a semplici scambi di testo. Oggi, ci sono altre alternative per comunicare con un server, come l'uso di WebSocket o gRPC. Tuttavia, l'invio di richieste HTTP rimane uno dei metodi più comuni e affidabili per comunicare con un server.

Per implementare il modulo `HTTPoison`, è stato utilizzato il client HTTP Erlang `ibrowse`. Questo consente ad Elixir di supportare facilmente il protocollo HTTP. Tuttavia, ci sono anche altri moduli Elixir disponibili per l'invio di richieste HTTP, come `Tesla` e `Finch`.

## Vedi anche:
- Documentazione ufficiale del modulo HTTPoison: https://hexdocs.pm/httpoison/HTTPoison.html
- Documentazione ufficiale del linguaggio Elixir: https://elixir-lang.org/
- Alternativa ad Elixir, come Node.js, per l'invio di richieste HTTP: https://nodejs.org/en/