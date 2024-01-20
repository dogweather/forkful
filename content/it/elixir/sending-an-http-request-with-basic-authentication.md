---
title:                "Inviare una richiesta http con autenticazione di base"
html_title:           "Bash: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Invio di una richiesta HTTP con autenticazione di base in Elixir

## Cosa & Perché?

L'invio di una richiesta HTTP con autenticazione di base è un'operazione che permette a un programma di comunicare su internet in modo sicuro. I programmatori lo fanno per interagire con le API protette e per gestire i dati web.

## Come fare:

In Elixir, usiamo la libreria HTTPoison. Aggiungi HTTPoison alla tua lista di dipendenze nel tuo `mix.exs`:

```elixir
def deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end
```

Poi esegui `mix deps.get` nel terminale per scaricare HTTPoison. Una volta che ti sei assicurato di averlo, ecco come inviare una richiesta GET con l'autenticazione di base:

```elixir
defmodule MyModule do
  require HTTPoison

  def get_my_data do
    url = "http://example.com"
    headers = ["Authorization": "Basic " <> :base64.encode_to_string("user:pass")]
    HTTPoison.get(url, headers)
  end
end
```

La risposta sarà qualcosa del genere:

```elixir
{:ok,
 %HTTPoison.Response{
   body: "[...]",
   headers: [...],
   request: %HTTPoison.Request{...},
   request_url: "http://example.com",
   status_code: 200
 }}
```

## Approfondimento:

L'autenticazione di base HTTP è stato introdotto per la prima volta nel RFC 761 nel 1980, ma poiché trasporta le credenziali come testo non cifrato (base64 non è una cifratura), non è consigliato per i protocolli HTTP non crittografati.

Come alternativa, puoi usare l'autenticazione digest o il token JWT.

In HTTPoison, l'header "Authorization" contiene la parola "Basic" seguita da una stringa codificata in base64 del formato "username:password". Keep-alive e l'uso di pool di connessioni vengono gestiti automaticamente da Hackney, la libreria sottostante.

## Guarda Anche:

- Documentazione di HTTPoison: https://hexdocs.pm/httpoison/readme.html
- Ulteriori dettagli sull'autenticazione di base: https://tools.ietf.org/html/rfc7617
- Una guida alla crittografia base64: https://www.base64encode.org/ 
- Hackney: https://github.com/benoitc/hackney