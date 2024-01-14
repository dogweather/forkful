---
title:                "Elixir: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Perché

Invio di una richiesta HTTP è un processo essenziale nello sviluppo di applicazioni web. Attraverso le richieste HTTP, è possibile comunicare con server esterni, ottenere risposte e utilizzare i dati ricevuti per creare un'esperienza utente dinamica. In quest'articolo, impareremo come inviare una richiesta HTTP utilizzando Elixir.

## Come fare

Per inviare una richiesta HTTP in Elixir, utilizzeremo il modulo `HTTPoison` della libreria `HTTPoison`. Prima di tutto, è necessario aggiungere `HTTPoison` come dipendenza nel tuo file `mix.exs`.

```
defdeps do
  [{:httpoison, "~> 1.6"}]
end
```

Successivamente, è necessario importare il modulo all'interno del tuo modulo principale.

```elixir
defmodule MyApp do
  import HTTPoison
end
```

Per inviare una richiesta GET a un URL specifico, utilizziamo la funzione `get/2` fornita da `HTTPoison`. Ad esempio, per ottenere i dati di un esempio di API di libri, possiamo fare quanto segue:

```elixir
iex> response = get("https://www.example.com/books")
%HTTPoison.Response{ ... }
```

L'oggetto `response` conterrà i dati ricevuti dal server. Possiamo quindi estrarre i dati dal corpo della risposta utilizzando la funzione `body/1`.

```elixir
iex> body = body(response)
"{\"books\": [{\"title\": \"The Alchemist\", \"author\": \"Paulo Coelho\"}, {\"title\": \"To Kill a Mockingbird\", \"author\": \"Harper Lee\"}]}"
```

## Approfondimento

Esistono diverse opzioni per configurare e personalizzare le richieste HTTP con `HTTPoison`. Ad esempio, è possibile impostare header personalizzati, parametri di query o impostare il timeout della richiesta. È importante consultare la documentazione di `HTTPoison` per ulteriori informazioni su come utilizzare tutti i suoi metodi e opzioni.

## Vedi anche

- Documentazione di `HTTPoison`: https://hexdocs.pm/httpoison/1.6.0/api-reference.html
- Tutorial su `HTTPoison` di Elixir School: https://elixirschool.com/it/lessons/specifics/http/
- Articolo su come gestire le richieste HTTP asincrone in Elixir: https://blog.appsignal.com/2019/04/16/asynchronous-http-requests-in-elixir.html