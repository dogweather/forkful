---
date: 2024-01-20 18:02:20.178057-07:00
description: "How to: Per fare una richiesta HTTP con autenticazione di base in Ruby,\
  \ possiamo usare la gemma 'net/http'. Nota che dovresti usare le tue credenziali\u2026"
lastmod: '2024-03-13T22:44:44.051243-06:00'
model: gpt-4-1106-preview
summary: Per fare una richiesta HTTP con autenticazione di base in Ruby, possiamo
  usare la gemma 'net/http'.
title: Inviare una richiesta http con autenticazione di base
weight: 45
---

## How to:
Per fare una richiesta HTTP con autenticazione di base in Ruby, possiamo usare la gemma 'net/http'. Nota che dovresti usare le tue credenziali reali.

```Ruby
require 'net/http'
require 'uri'

uri = URI('http://example.com')
username = 'mario'
password = 'segreto'

# Crea un oggetto di richiesta
request = Net::HTTP::Get.new(uri)
request.basic_auth(username, password)

# Esegui la richiesta
response = Net::HTTP.start(uri.hostname, uri.port) {|http|
  http.request(request)
}

puts response.body # Stampa il corpo della risposta
```

Se il server risponde con successo, vedrai il contenuto protetto. Altrimenti, riceverai un errore di autenticazione.

## Deep Dive
L'autenticazione di base HTTP è semplice ma non la più sicura. Invia credenziali in plain text (base64 encoded, ma facilmente decodificabile), quindi dovresti sempre usarla con HTTPS. 

Storicamente, era un metodo comune per controllare l'accesso, ma ora spesso viene sostituito da token o Oauth per maggiore sicurezza. 

In Ruby, la libreria 'net/http' è standard ma ci sono alternative come 'HTTParty' o 'Faraday' che rendono il codice più leggero o offrono funzionalità avanzate.

```Ruby
# Esempio con HTTParty
require 'httparty'

auth = {username: 'mario', password: 'segreto'}
response = HTTParty.get('http://example.com', basic_auth: auth)

puts response.body
```

Usa l'autenticazione di base solo per prototipi rapidi o internamente, dove si può garantire la confidenzialità della connessione.

## See Also
- Ruby Doc per Net::HTTP: https://ruby-doc.org/stdlib/libdoc/net/http/rdoc/Net/HTTP.html
- HTTParty Gem: https://github.com/jnunemaker/httparty
- Faraday Gem: https://github.com/lostisland/faraday
- RFC 7617, 'The 'Basic' HTTP Authentication Scheme': https://tools.ietf.org/html/rfc7617
