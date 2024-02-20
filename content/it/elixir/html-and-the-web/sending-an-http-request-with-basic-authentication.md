---
date: 2024-01-20 18:01:25.080218-07:00
description: "Inviare una richiesta HTTP con autenticazione di base significa includere\
  \ le credenziali di accesso (username e password) nell'header HTTP per accedere\
  \ a\u2026"
lastmod: 2024-02-19 22:05:02.193361
model: gpt-4-1106-preview
summary: "Inviare una richiesta HTTP con autenticazione di base significa includere\
  \ le credenziali di accesso (username e password) nell'header HTTP per accedere\
  \ a\u2026"
title: Inviare una richiesta http con autenticazione di base
---

{{< edit_this_page >}}

## Cosa & Perché?
Inviare una richiesta HTTP con autenticazione di base significa includere le credenziali di accesso (username e password) nell'header HTTP per accedere a risorse protette. I programmatori lo fanno quando devono interagire con API o servizi web che richiedono autenticazione per garantire protezione e controllo degli accessi.

## Come fare:
Elixir usa la libreria `HTTPoison` per fare richieste HTTP con autenticazione di base in modo semplice. Installa prima la libreria aggiungendo `{:httpoison, "~> 1.8"}` al tuo `mix.exs` e poi esegui `mix deps.get`. Ecco un esempio di come usarla:

```elixir
defmodule HTTPExample do
  def send_basic_auth_request do
    url = "https://example.com/protected/resource"
    username = "user"
    password = "pass"
    options = [basic_auth: {username, password}]

    HTTPoison.get(url, [], options)
  end
end

# Uso della funzione
{:ok, response} = HTTPExample.send_basic_auth_request()

IO.inspect response.status_code  # Potrebbe stampare: 200
IO.inspect response.body        # Il corpo della risposta (se l'autenticazione è andata a buon fine)
```

## Approfondimento
L'autenticazione di base HTTP ha origini negli albori del web (RFC 7617) per proteggere risorse web senza dover implementare sistemi complessi. Non è la scelta migliore per la sicurezza moderna dato che le credenziali viaggiano codificate in base64 e non criptate. Alternativamente, si può usare l'autenticazione Digest o OAuth per maggiore sicurezza. Riguardo all'implementazione in Elixir, HTTPoison è un comodo wrapper intorno a `hackney`, che gestisce la connessione e la codifica delle richieste HTTP.

## Vedi anche
- HTTPoison GitHub: https://github.com/edgurgel/httpoison
- Hackney library GitHub: https://github.com/benoitc/hackney
- Specifiche RFC 7617 per l'autenticazione di base HTTP: https://tools.ietf.org/html/rfc7617
