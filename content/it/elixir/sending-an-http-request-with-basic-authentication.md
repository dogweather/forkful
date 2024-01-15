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

## Perché

Molti servizi web richiedono una forma di autenticazione per accedere alle loro risorse. L'utilizzo di basic authentication con le richieste HTTP è un modo semplice ed efficace per fornire credenziali di accesso e garantire la sicurezza delle comunicazioni.

## Come

Per inviare una richiesta HTTP con basic authentication in Elixir, dobbiamo prima importare il modulo HTTPoison e creare una mappa con le nostre credenziali:

```Elixir
params = %{username: "username", password: "password"}
```

Successivamente, possiamo utilizzare la funzione `get/3` per effettuare una richiesta GET a un determinato URL, passando la mappa come terzo argomento:

```Elixir
response = HTTPoison.get("https://www.example.com", params, [basic_auth: params])
```

Il risultato sarà un oggetto `{:ok, response}` contenente la risposta HTTP ricevuta dal server. Possiamo quindi accedere al corpo della risposta utilizzando la funzione `response.body`.

## Deep Dive

Per inviare una richiesta con basic authentication, è necessario che il server supporti questo tipo di autenticazione. Inoltre, è importante notare che le credenziali vengono inviate in chiaro, quindi è consigliabile utilizzare sempre connessioni HTTPS per garantire la sicurezza delle comunicazioni.

Inoltre, è possibile specificare il tipo di autenticazione utilizzando l'opzione `mode` nella chiamata alla funzione `get/3`, ad esempio:

```Elixir
HTTPoison.get("https://www.example.com", params, [basic_auth: params, mode: :digest])
```

## See Also

- [Documentazione HTTPoison](https://hexdocs.pm/httpoison/)
- [Specifiche basic authentication HTTP](https://www.rfc-editor.org/rfc/rfc2617)