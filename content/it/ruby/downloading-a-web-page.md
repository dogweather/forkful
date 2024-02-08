---
title:                "Scaricare una pagina web"
aliases:
- it/ruby/downloading-a-web-page.md
date:                  2024-01-20T17:44:40.142053-07:00
model:                 gpt-4-1106-preview
simple_title:         "Scaricare una pagina web"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?
Scaricare una pagina web significa portare il contenuto di quella pagina sul tuo computer. I programmatori lo fanno per analizzare i dati, testare la disponibilità dei siti o automatizzare attività legate al web.

## How to:
Usando Ruby, possiamo scaricare una pagina web con gemme come `open-uri` per un approccio semplice o `net/http` per uno più dettagliato.

```Ruby
require 'open-uri'

# Usa 'open-uri' per aprire l'URL desiderato
page_content = URI.open('https://www.example.com').read

puts page_content[0..200] # stampiamo solo i primi 200 caratteri per brevità
```

Questo esempio stamperà i primi 200 caratteri del codice HTML della pagina example.com.

Con `net/http`:

```Ruby
require 'net/http'
require 'uri'

# Imposta l'URI della pagina da scaricare
url = URI.parse('https://www.example.com')

# Crea una richiesta HTTP per ottenere la risposta del server
response = Net::HTTP.get_response(url)

puts response.body[0..200] # anche qui vediamo solo i primi 200 caratteri
```

Questo codice fa la stessa cosa, ma con maggiore controllo su autenticazione, redirection, e headers.

## Deep Dive
Storicamente, scaricare una pagina web significava inviare una richiesta HTTP e ricevere un codice HTML di risposta. Le gemme Ruby come `open-uri` e `net/http` simplificano questo processo. 

`open-uri` è più diretto e fa il lavoro con meno configurazione, consigliato per script piccoli e semplici. `net/http`, d'altra parte, offre più opzioni di personalizzazione per cose come impostare timeout o gestire HTTPS.

In Ruby on Rails, potresti usare gemme come `Mechanize`, che simula un browser web e può gestire cookie, sessioni e javascript. Altra alternativa è `HTTParty`, che rende il processo così semplice come scrivere `HTTParty.get(url)`.

Si possono incontrare sfide come la gestione delle richieste AJAX o le pagine che richiedono JS per la visualizzazione completa. In questi casi, strumenti come Selenium o Watir, che automatizzano un browser, possono essere necessari.

## See Also
- [Ruby Doc: open-uri](https://ruby-doc.org/stdlib-3.0.0/libdoc/open-uri/rdoc/OpenURI.html)
- [Ruby Doc: net/http](https://ruby-doc.org/stdlib-3.0.0/libdoc/net/http/rdoc/Net/HTTP.html)
- [Mechanize](https://github.com/sparklemotion/mechanize)
- [HTTParty](https://github.com/jnunemaker/httparty)
- [Selenium](https://www.selenium.dev/documentation/en/)
- [Watir](http://watir.com/)
