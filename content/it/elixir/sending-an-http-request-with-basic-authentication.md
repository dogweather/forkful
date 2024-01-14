---
title:                "Elixir: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Perchè

Se sei un programmatore Elixir e stai lavorando su un'applicazione web, è probabile che prima o poi ti troverai a dover inviare una richiesta HTTP con autenticazione di base. Ma perché dovresti farlo? In breve, l'autenticazione di base è un meccanismo di sicurezza che consente di accedere a risorse protette tramite l'inserimento di credenziali (username e password). In questo articolo, esploreremo come inviare una richiesta HTTP con autenticazione di base utilizzando Elixir.

## Come

Per inviare una richiesta HTTP con autenticazione di base in Elixir, abbiamo bisogno di utilizzare il modulo HTTPoison (disponibile su hex.pm). Iniziamo dalla creazione di un'istanza di HTTPoison, passando l'URL di destinazione e le credenziali di accesso come parametri:

```Elixir
http = HTTPoison.BasicAuth.init("https://www.example.com", "username", "password")
```

Successivamente, possiamo creare una richiesta GET utilizzando la funzione `get/2` e passando l'istanza di HTTPoison come primo parametro:

```Elixir
response = HTTPoison.get(http, "/api/some_endpoint")
```

Infine, possiamo controllare lo stato della richiesta e stampare il contenuto della risposta utilizzando la funzione `status/1` e `body/1`:

```Elixir
response.status # 200 OK
response.body # {"data": "Lorem ipsum"}
```

## Deep Dive

Il modulo HTTPoison facilita l'invio di richieste HTTP con autenticazione di base in Elixir. Oltre alla funzione `get/2` utilizzata nell'esempio precedente, possiamo anche utilizzare le funzioni `post/3`, `put/3` e `delete/3` per le richieste POST, PUT e DELETE rispettivamente. Inoltre, possiamo specificare opzioni aggiuntive come l'intestazione della richiesta e il corpo della richiesta utilizzando i parametri opzionali `headers` e `body`. Puoi trovare maggiori informazioni sulla sintassi e le opzioni di HTTPoison nella [documentazione ufficiale](https://hexdocs.pm/httpoison/HTTPoison.Base.html).

## Vedi Anche

Se vuoi saperne di più sull'autenticazione di base e sulle richieste HTTP in Elixir, puoi consultare questi siti:

- [Documentazione ufficiale di Elixir](https://elixir-lang.org/getting-started/introduction.html)
- [Documentazione ufficiale di HTTPoison](https://hexdocs.pm/httpoison/HTTPoison.html)
- [Articolo su HTTP requests in Elixir](https://gomex.me/2016/08/09/elixir-web-requests/)