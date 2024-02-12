---
title:                "Een HTTP-verzoek verzenden"
aliases:
- /nl/ruby/sending-an-http-request/
date:                  2024-01-28T22:07:51.071997-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een HTTP-verzoek verzenden"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/ruby/sending-an-http-request.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Een HTTP-verzoek verzenden betekent dat je data opvraagt van een bron op het web. Programmeurs doen dit om te communiceren met API's, webinhoud te schrapen of om met servers te praten.

## Hoe te:

Ruby maakt het vrij eenvoudig om HTTP-verzoeken te verzenden. Hier is de snelste manier met de standaardbibliotheek Net::HTTP.

```Ruby
require 'net/http'
require 'uri'

uri = URI('http://example.com')
response = Net::HTTP.get(uri)
puts response
```

Dit zal de HTML-inhoud van `http://example.com` uitprinten.

Misschien wil je ook data posten:

```Ruby
require 'net/http'
require 'uri'

uri = URI('http://example.com/api')
res = Net::HTTP.post_form(uri, 'key1' => 'value1', 'key2' => 'value2')
puts res.body
```

Dit verzendt een POST-verzoek met data en toont de reactie.

## Diepgaand:

In het verleden was het verzenden van HTTP-verzoeken omslachtiger, en moest je mogelijk een gem zoals `HTTParty` gebruiken. Maar Ruby's ingebouwde `Net::HTTP` bibliotheek is enorm verbeterd. Het ondersteunt nu de meeste dingen die je nodig zult hebben.

Maar, `Net::HTTP` kan langdradig zijn. Als je project meer HTTP-functies of syntactische suiker nodig heeft, zijn `HTTParty` of `Faraday` geweldige alternatieven. Deze gems bieden een meer expressieve API en kunnen complexere scenario's aan zoals middleware of verschillende adapters.

Fundamenteel gaat het verzenden van een HTTP-verzoek met Ruby over het creëren van een HTTP-client, het instellen van een verzoekobject met methode, headers en indien nodig een body, en vervolgens het versturen van het verzoek om een reactie te ontvangen.

HTTParty voorbeeld:

```Ruby
require 'httparty'

response = HTTParty.get('http://example.com')
puts response.body
```

Dit doet hetzelfde als `Net::HTTP.get`, maar met minder configuratie.

## Zie ook:

Voor meer gedetailleerde info zijn de documenten van Ruby erg behulpzaam:
- Net::HTTP: https://ruby-doc.org/stdlib/libdoc/net/http/rdoc/Net/HTTP.html
- HTTParty: https://github.com/jnunemaker/httparty
- Faraday: https://lostisland.github.io/faraday/

En als je een grote interesse hebt in Ruby's HTTP-netwerkwerk, neem dan eens een kijkje op:
- Ruby's Open URI: https://ruby-doc.org/stdlib/libdoc/open-uri/rdoc/OpenURI.html
- WebMock voor het testen van HTTP-verzoeken: https://github.com/bblimke/webmock
