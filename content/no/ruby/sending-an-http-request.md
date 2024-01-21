---
title:                "Å sende en HTTP-forespørsel"
date:                  2024-01-20T18:00:31.977564-07:00
model:                 gpt-4-1106-preview
simple_title:         "Å sende en HTTP-forespørsel"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?
Å sende en HTTP-forespørsel er å be en webserver om data eller om å utføre en handling. Programmerere gjør dette for å integrere med webtjenester, laste ned innhold eller sende data til en server.

## How to:
Ruby har flere måter å sende HTTP-forespørsler på. Her er et enkelt eksempel med `net/http` biblioteket:

```Ruby
require 'uri'
require 'net/http'

uri = URI('http://www.example.com/index.html')
response = Net::HTTP.get_response(uri)

puts response.body if response.is_a?(Net::HTTPSuccess)
```

Output:
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```
Eller med `httparty` gem:

```Ruby
require 'httparty'

response = HTTParty.get('http://www.example.com/index.html')

puts response.body if response.success?
```

## Deep Dive
Før Ruby hadde innebygde biblioteker for nettverksforespørsler, måtte programmere bruke socket-programmering for hånd. `net/http` kom og forenklet prosessen. Alternativer som `httparty` og `faraday` tilbyr mer funksjonalitet og syntaktisk sukker. Disse gemmene pakker `net/http`'s funksjonalitet og gir ekstra features som middleware-støtte og enklere testing.

Når du sender HTTP-forespørsler, er det viktig å håndtere nettverksfeil og dårlige serverresponser. Noen detaljer, som tid ut (timeout) eller lesing av store svar, kan kreve ytterligere konfigurasjon.

## See Also
- Ruby’s Net::HTTP documentation: https://ruby-doc.org/stdlib-2.7.1/libdoc/net/http/rdoc/Net/HTTP.html
- HTTParty GitHub repository: https://github.com/jnunemaker/httparty
- Faraday GitHub repository: https://github.com/lostisland/faraday
- Forståelse av HTTP: https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview