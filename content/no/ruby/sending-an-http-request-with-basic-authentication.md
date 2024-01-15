---
title:                "Å sende en http forespørsel med grunnleggende autentisering"
html_title:           "Ruby: Å sende en http forespørsel med grunnleggende autentisering"
simple_title:         "Å sende en http forespørsel med grunnleggende autentisering"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Hvorfor
Å sende en HTTP-forespørsel med grunnleggende autentisering er en vanlig måte å sikre server-til-server kommunikasjon på. Dette er spesielt nyttig hvis du trenger å få tilgang til beskyttede ressurser på et nettsted eller API.

# Hvordan gjøre det
```Ruby
require 'net/http'

# Opprett en Net::HTTP-objekt med URLen til APIet du vil sende en forespørsel til
uri = URI("https://api.example.com")

# Opprett et HTTP-forespørsel objekt med et passord og brukernavn
request = Net::HTTP::Get.new(uri)
request.basic_auth("brukernavn", "passord")

# Send forespørselen og lagre svaret i en variabel
response = Net::HTTP.start(uri.hostname, uri.port, use_ssl: true) do |http|
  http.request(request)
end

# Skriv ut svaret
puts response.body
```

# Dypdykk
Når du bruker grunnleggende autentisering, sender du brukernavnet og passordet ditt over i en HTTP-forespørsel via en base64-kodet streng. Dette bør bare gjøres over en sikker og kryptert forbindelse for å beskytte brukernavn og passord fra å bli avlyttet.

# Se også
- [Net::HTTP ruby documentation](https://ruby-doc.org/stdlib-3.0.2/libdoc/net/http/rdoc/Net/HTTP.html)
- [Basic Access Authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)