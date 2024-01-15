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

## Perché

Se stai sviluppando un'applicazione web o un'API, potresti aver bisogno di autenticazione di base per proteggerla e limitare l'accesso solo agli utenti autorizzati. Invio di una richiesta HTTP con autenticazione di base è un modo semplice ed efficace per garantire la sicurezza delle tue risorse.

## Come fare

Per inviare una richiesta HTTP con autenticazione di base in Ruby, puoi utilizzare la libreria ```Net::HTTP``` inclusa nella libreria standard di Ruby. Ecco un esempio di come creare un'istanza di ```Net::HTTP``` e inviare una richiesta GET con autenticazione di base:

```
uri = URI('http://esempio.com')
http = Net::HTTP.new(uri.host, uri.port)
request = Net::HTTP::Get.new(uri)
request.basic_auth('nome_utente', 'password')
response = http.request(request)
puts response.body 
```

Il codice sopra invia una richiesta GET all'URL fornito e passa le credenziali di autenticazione tramite il metodo ```basic_auth```. Se la richiesta ha successo, il corpo della risposta verrà stampato a schermo.

## Approfondimenti

Per comprendere meglio come funziona l'autenticazione di base in una richiesta HTTP, è utile esaminare il formato dei dati che vengono inviati. L'autenticazione di base richiede che le credenziali siano codificate in Base64 e inserite all'interno dell'header ```Authorization```. Questo processo di codifica rende le credenziali leggibili solo al server e non ai possibili spettatori delle richieste HTTP. Puoi saperne di più sulla codifica Base64 e sull'header Authorization nella documentazione ufficiale di HTTP.

## Vedi anche

- [Documentazione ufficiale di HTTP](https://tools.ietf.org/html/rfc2617)
- [Libreria Net::HTTP di Ruby](https://ruby-doc.org/stdlib-2.7.2/libdoc/net/http/rdoc/Net/HTTP.html)