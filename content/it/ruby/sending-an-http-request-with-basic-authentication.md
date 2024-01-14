---
title:                "Ruby: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Perché 

Molti sviluppatori utilizzano le richieste HTTP con autenticazione di base per accedere e scambiare dati con applicazioni web. Questo può essere utile per creare integrazioni tra diversi sistemi o per accedere a risorse protette su un server. In questo articolo, esploreremo come inviare una richiesta HTTP con autenticazione di base utilizzando Ruby.

## Come Fare 

Per inviare una richiesta HTTP con autenticazione di base, possiamo utilizzare la libreria Net::HTTP di Ruby. Iniziamo importando la libreria nel nostro codice:

```Ruby
require 'net/http'
```

Successivamente, dobbiamo creare una nuova istanza della classe Net::HTTP, specificando l'URL del server a cui vogliamo fare la richiesta:

```Ruby
uri = URI('https://www.example.com') # sostituisci con l'URL desiderato
http = Net::HTTP.new(uri.host, uri.port)
```

A questo punto, dobbiamo creare una nuova istanza della classe Net::HTTP::Get, specificando l'endpoint a cui vogliamo fare la richiesta:

```Ruby
request = Net::HTTP::Get.new(uri.request_uri) # sostituisci con l'endpoint desiderato
```

Ora, dobbiamo aggiungere le credenziali per l'autenticazione di base alla nostra richiesta. Per fare ciò, utilizziamo il metodo base64.encode64 per codificare il nome utente e la password in base64, e poi impostiamo l'header Authorization sulla nostra richiesta:

```Ruby
username = "username" # sostituisci con il tuo nome utente
password = "password" # sostituisci con la tua password
auth = "Basic " + Base64.encode64("#{username}:#{password}")
request["Authorization"] = auth
```

Infine, possiamo effettuare la richiesta al server e ottenere la risposta:

```Ruby
response = http.request(request)
puts response.body # stampa il corpo della risposta
```

Con questo codice, dovresti essere in grado di inviare una richiesta HTTP con autenticazione di base e ottenere una risposta dal server.

## Approfondimento

L'autenticazione di base è uno dei vari metodi di autenticazione che possono essere utilizzati nelle richieste HTTP. Questo metodo richiede che il client includa le credenziali di accesso (nome utente e password) nella richiesta, codificate in base64. Tuttavia, questo metodo è considerato non sicuro perché le credenziali possono essere facilmente decodificate e lette. Alcuni dei metodi di autenticazione più sicuri includono OAuth e JSON Web Tokens (JWT).

## Vedi Anche

- [Documentazione di Net::HTTP](https://ruby-doc.org/stdlib-2.6.4/libdoc/net/http/rdoc/Net/HTTP.html)
- [Panoramica sui metodi di autenticazione HTTP](https://developer.mozilla.org/it/docs/Web/HTTP/Authentication) 
- [Esempi di autenticazione HTTP con Ruby](https://www.rubyguides.com/2018/08/ruby-http-authentication/)