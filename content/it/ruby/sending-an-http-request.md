---
title:                "Inviare una richiesta http"
html_title:           "C++: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Invio di una richiesta HTTP con Ruby 

## Cos'è e perché?

L'invio di una richiesta HTTP è l'azione di richiedere dati da un server attraverso internet. I programmatori lo fanno per interagire con API, scaricare file, inviare dati ed eseguire altre operazioni basate su rete.

## Come fare:

Ruby rende molto semplice l'invio di richieste HTTP attraverso il modulo 'net/http'. Ecco un esempio di semplice invio di una richiesta GET:

```Ruby
require 'net/http'

url = URI("http://example.com/")
response = Net::HTTP.get_response(url)

puts response.body
```

In questo esempio, cerchiamo prima di ottenere il modulo net/http, creare un URL e quindi usare il metodo 'get_response' per inviare la richiesta GET e ricevere una risposta.

## Approfondimenti:

Invio di richieste HTTP risale all'invenzione del protocollo HTTP stesso, che è la base del web come lo conosciamo. Ci sono numerose alternative al modulo 'net/http' in Ruby, tra cui 'httparty' e 'rest-client', che offrono funzionalità extra e un'interfaccia più pulita.

L'invio di una richiesta HTTP inizia con la creazione di una connessione TCP a un server su una specifica porta (di solito la 80 per HTTP o la 443 per HTTPS). Poi, viene inviato un messaggio HTTP che dice al server quale risorsa si vuole ottenere o modificare. Infine, il server risponde con un messaggio HTTP che include sia un codice di stato per indicare se la richiesta è stata un successo o un errore, sia eventualmente il contenuto richiesto.

## Vedi anche:

1. [Documentazione ufficiale di 'net/http'](https://ruby-doc.org/stdlib-2.7.0/libdoc/net/http/rdoc/Net/HTTP.html)
2. ['httparty'](https://github.com/jnunemaker/httparty)
3. ['rest-client'](https://github.com/rest-client/rest-client)