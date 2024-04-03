---
date: 2024-01-20 17:44:36.351315-07:00
description: "\xC5 laste ned en webside betyr \xE5 hente HTML-innholdet fra en URL\
  \ og lagre det p\xE5 din egen datamaskin. Programmerere gj\xF8r dette for \xE5 analysere\
  \ innholdet,\u2026"
lastmod: '2024-03-13T22:44:41.316886-06:00'
model: gpt-4-1106-preview
summary: "\xC5 laste ned en webside betyr \xE5 hente HTML-innholdet fra en URL og\
  \ lagre det p\xE5 din egen datamaskin."
title: Nedlasting av en nettside
weight: 42
---

## Slik gjør du:
Ruby gjør det enkelt å laste ned en nettside. Her er et kort eksempel ved bruk av `net/http`-biblioteket:

```ruby
require 'net/http'
require 'uri'

uri = URI('https://www.example.com')
response = Net::HTTP.get(uri)

puts response # Skriver ut HTML-innholdet til konsollen
```

Sample output:
```
<!doctype html>
<html>
<head>
    <title>Eksempel Domene</title>
...
</html>
```

## Dypdykk:
Tidligere var det vanlig å bruke biblioteker som `open-uri` for å laste ned nettsider, men `net/http` har blitt standard for å håndtere HTTP-forespørsler i Ruby. Alternativer inkluderer eksterne gems som `httparty` eller `rest-client` for de som ønsker ekstra funksjonalitet eller et mer forenklet API.

Implementering med `net/http` gir god kontroll over HTTP-forespørsler, men kan virke overveldende for nye brukere. Det er grunnen til populariteten til alternativene som tilbyr en høyere abstraksjonsnivå.

## Se Også:
- Ruby's Net::HTTP dokumentasjon: https://ruby-doc.org/stdlib-3.1.0/libdoc/net/http/rdoc/Net/HTTP.html
- HTTParty gem: https://github.com/jnunemaker/httparty
- RestClient gem: https://github.com/rest-client/rest-client
- Web scraping guide for Ruby: https://www.nokogiri.org/tutorials/parsing_an_html_xml_document.html
