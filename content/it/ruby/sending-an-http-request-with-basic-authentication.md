---
title:                "Inviare una richiesta http con autenticazione di base"
html_title:           "Ruby: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

Cosa & Perché? 

In poche parole, l'invio di una richiesta HTTP con autenticazione di base è un modo per accedere a un server web protetto inserendo un nome utente e una password. I programmatori utilizzano questo metodo quando hanno bisogno di accedere a risorse riservate o protette, come ad esempio un'API.

Come fare: 

Utilizzando la libreria standard di Ruby "Net::HTTP", è possibile inviare una richiesta HTTP con autenticazione di base semplicemente specificando l'header "Authorization" con le credenziali desiderate nel seguente modo:

```ruby
require 'net/http'

uri = URI('http://www.example.com')
req = Net::HTTP::Get.new(uri)

# Inserisci la tua password
req.basic_auth 'username', 'password'

res = Net::HTTP.start(uri.hostname, uri.port) {|http|
  http.request(req)
}

puts res.body
```

Output: 

La risposta della richiesta sarà una stringa contenente il corpo della risorsa richiesta, o un codice di errore se le credenziali fornite non sono valide.

Deep Dive: 

L'autenticazione di base è uno dei metodi di autenticazione più antichi e semplici, che fa parte delle specifiche HTTP 1.0 del 1996. Questo metodo trasmette le credenziali utilizzando un codice di autorizzazione codificato in base64 all'interno dell'header "Authorization". Tuttavia, a causa della mancanza di sicurezza dei dati trasmesse, è stato sostituito da metodi di autenticazione più sicuri come l'autenticazione digest o l'utilizzo di certificati SSL.

See Also: 

Per approfondire su come gestire l'autenticazione di base in Ruby, puoi consultare la documentazione ufficiale della classe "Net::HTTP" (https://ruby-doc.org/stdlib-2.7.2/libdoc/net/http/rdoc/index.html). In alternativa, puoi utilizzare anche altre librerie di terze parti come "rest-client" o "faraday".