---
title:                "Elixir: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Perché

Scaricare una pagina web è un'operazione fondamentale per la maggior parte delle applicazioni web. Ci consente di ottenere informazioni, dati e contenuti in modo rapido e facile, rendendo la nostra esperienza utente più efficiente.

## Come Fare

Per scaricare una pagina web in Elixir, possiamo utilizzare la libreria standard HTTPoison. Iniziamo prima importando la libreria nel nostro progetto:

```Elixir
def deps do
  [
    {:httpoison, "~> 1.0"}
  ]
end
```

Una volta importata la libreria, possiamo utilizzare la sua funzione `get!/2` per ottenere il contenuto di una pagina web. Ad esempio, se vogliamo scaricare il contenuto della pagina principale di Google, possiamo fare quanto segue:

```Elixir
{:ok, %{status_code: 200, body: body}} = HTTPoison.get!("https://www.google.com")
```

La funzione `get!/2` restituirà un'ottima tupla contenente lo stato della richiesta e il corpo della risposta. Nel nostro esempio, stiamo assegnando il corpo (ovvero il contenuto della pagina web) alla variabile `body`.

Possiamo anche impostare delle opzioni aggiuntive per la nostra richiesta, come ad esempio specificare parametri GET o aggiungere header personalizzati. Tutto ciò è possibile grazie alla struttura opzionale `opts` del `get!/2`.

Ora che abbiamo il contenuto della pagina web, possiamo utilizzare le funzioni della libreria standard Elixir per elaborare e analizzare il testo. Ad esempio, possiamo utilizzare le funzioni `String.split/3` o `Regex.run/3` per estrarre informazioni specifiche dalla pagina.

## Approfondimento

Scaricare una pagina web è un'operazione relativamente semplice, ma ci sono alcuni aspetti da considerare per renderla più efficiente. Ad esempio, possiamo utilizzare le funzioni della libreria standard `Stream` per elaborare i dati in modo più veloce e meno oneroso in termini di memoria.

Inoltre, possiamo anche utilizzare il modulo `HTTPoison.Async` per eseguire richieste asincrone e gestire più pagine web contemporaneamente. Ciò può essere utile nel caso in cui stiamo elaborando un grande numero di pagine web o se vogliamo ridurre il tempo di attesa per il completamento delle richieste.

## Vedi Anche

- [Documentazione HTTPoison](https://hexdocs.pm/httpoison/HTTPoison.html#get!/2)
- [Pagina GitHub ufficiale di HTTPoison](https://github.com/edgurgel/httpoison)
- [Tutorial sull'elaborazione dei dati in Elixir con Stream](https://elixirschool.com/lessons/advanced/stream/)
- [Articolo su richieste asincrone in Elixir con HTTPoison](https://medium.com/blackode/asynchronous-http-in-elixir-using-httpoison-a7162bba4137)