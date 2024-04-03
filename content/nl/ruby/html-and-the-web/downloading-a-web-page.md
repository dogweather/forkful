---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:32.735504-07:00
description: 'Hoe te: Ruby maakt het downloaden van een webpagina eenvoudig met bibliotheken
  zoals `net/http` en gems zoals `open-uri`. Zo doe je het met `net/http`.'
lastmod: '2024-03-13T22:44:51.336738-06:00'
model: gpt-4-0125-preview
summary: Ruby maakt het downloaden van een webpagina eenvoudig met bibliotheken zoals
  `net/http` en gems zoals `open-uri`.
title: Een webpagina downloaden
weight: 42
---

## Hoe te:
Ruby maakt het downloaden van een webpagina eenvoudig met bibliotheken zoals `net/http` en gems zoals `open-uri`. Zo doe je het met `net/http`:

```Ruby
require 'net/http'
require 'uri'

url = URI.parse('http://example.com') 
response = Net::HTTP.get_response(url)

puts response.body if response.is_a?(Net::HTTPSuccess)
```

Je krijgt de HTML-inhoud van `http://example.com` uitgeprint.

Gebruik van `open-uri` is nog eenvoudiger:

```Ruby
require 'open-uri'

downloaded_page = URI.open('http://example.com').read
puts downloaded_page
```

Opnieuw wordt de inhoud van de webpagina op je terminal weergegeven.

## Diepgaande Duik
Terug in de vroege dagen van het web was het downloaden van een pagina wat arbeidsintensiever, waarbij het ambachtelijk vervaardigen van HTTP-verzoeken nodig was. Vandaag de dag abstraheert Ruby veel van die complexiteit weg.

Alternatieven voor `net/http` en `open-uri` omvatten hogere-niveau gems zoals `HTTParty` en `RestClient`. Ze bieden meer functies en een object-georiënteerde benadering. Voor zware web scraping, wenden veel Rubyisten zich tot `Nokogiri` om HTML te parsen of `Mechanize` dat zich gedraagt als een webbrowser.

Wat implementatie betreft, houd in gedachten dat `open-uri` een wrapper is voor `net/http`, dus het is vrij handig maar kan wat low-level controle missen. `net/http` geeft je meer controle over het verzoek maar kan uitgebreid zijn voor eenvoudige taken.

## Zie Ook
Voor verder lezen en aanvullende bronnen, bekijk:

- Ruby's Net::HTTP doc: [https://ruby-doc.org/stdlib-3.0.0/libdoc/net/http/rdoc/Net/HTTP.html](https://ruby-doc.org/stdlib-3.0.0/libdoc/net/http/rdoc/Net/HTTP.html)
- Open-URI doc: [https://ruby-doc.org/stdlib-3.0.0/libdoc/open-uri/rdoc/OpenURI.html](https://ruby-doc.org/stdlib-3.0.0/libdoc/open-uri/rdoc/OpenURI.html)
- Nokogiri's website: [https://nokogiri.org/](https://nokogiri.org/)
- Mechanize gem repository: [https://github.com/sparklemotion/mechanize](https://github.com/sparklemotion/mechanize)
- HTTParty gem op GitHub: [https://github.com/jnunemaker/httparty](https://github.com/jnunemaker/httparty)
- RestClient gem: [https://github.com/rest-client/rest-client](https://github.com/rest-client/rest-client)
