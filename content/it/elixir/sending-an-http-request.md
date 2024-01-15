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

## Perché

Se stai sviluppando un'applicazione web o un'API, probabilmente dovrai inviare richieste HTTP ad altre risorse. Usare Elixir per inviare queste richieste è una scelta ottimale grazie a librerie come HTTPoison e Tesla che semplificano il processo.

## Come Fare

Per inviare una richiesta HTTP con Elixir, è necessario prima importare la libreria HTTPoison nel tuo file. Puoi farlo con il seguente codice:

```Elixir
defp deps do
  [
    {:httpoison, "~> 1.6"}
  ]
end
```

Una volta importata la libreria, puoi creare la tua richiesta HTTP utilizzando la funzione `HTTPoison.get/2` e passando l'URL della risorsa come primo argomento. Se ad esempio vogliamo ottenere i dati di un utente dal seguente endpoint:

```
https://api.example.com/users/1
```

Possiamo utilizzare il seguente codice:

```Elixir
res = HTTPoison.get("https://api.example.com/users/1")
```

Questa funzione restituirà un'istanza della struct `HTTPoison.Response` contenente il codice di stato, l'header e il corpo della risposta ricevuta. Puoi accedere a questi valori utilizzando i seguenti metodi:

* `HTTPoison.Response.code`: restituisce il codice di stato della risposta.
* `HTTPoison.Response.headers`: restituisce un elenco di tuple contenente gli header della risposta.
* `HTTPoison.Response.body`: restituisce il corpo della risposta come una stringa.

Puoi anche passare un secondo argomento opzionale alla funzione `HTTPoison.get/2` per specificare alcuni parametri come header aggiuntivi, body della richiesta, timeout, etc.

## Approfondimenti

Per inviare una richiesta HTTP, Elixir utilizza la libreria hackney che, a sua volta, utilizza la libreria Erlang ibrowse. Queste librerie sono ampiamente utilizzate per la loro stabilità e prestazioni. Inoltre, è possibile gestire le richieste HTTP in modo asincrono utilizzando la libreria Tesla, che utilizza un pool di connessioni per migliorare le prestazioni. 

## Vedi Anche

* [Documentazione HTTPoison](https://hexdocs.pm/httpoison/api-reference.html)
* [Tutorial Tesla](https://sites.google.com/site/sergeyberezhnoy/h2-mobile-site/tesla-web)