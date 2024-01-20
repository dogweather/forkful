---
title:                "Sende en http-forespørsel med grunnleggende autentisering"
html_title:           "Kotlin: Sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Sende en http-forespørsel med grunnleggende autentisering"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

---

## Hva & Hvorfor?
Å sende en HTTP-forespørsel med grunnleggende autentisering er å sende en nettforespørsel som inneholder brukernavn og passord for å få tilgang til beskyttet informasjon. Programmerere gjør dette for å samhandle sikkert med webtjenester som krever innlogging.

## Hvordan gjør man det?
Her er et eksempel på hvordan du kan sende HTTP-forespørsel med grunnleggende autentisering ved bruk av Ruby's `net/http`-bibliotek.

```Ruby
require 'net/http'
require 'uri'

uri = URI.parse("https://eksempel.com/side")
request = Net::HTTP::Get.new(uri)
request.basic_auth("brukernavn", "passord")

response = Net::HTTP.start(uri.hostname, uri.port) do |http|
  http.request(request)
end

puts response.body
```
Dette eksemplet gjør en GET-forespørsel til "https://eksempel.com/side" ved bruk av brukernavn og passord for autentisering. Responsen er deretter skrevet ut på konsollen.

## Dypdykk
Grunnleggende autentisering er faktisk en av de eldste metodene for sikring av nettjenester, og den er fortsatt utbredt på grunn av dens enkelhet. Hver forespørsel du sender må inkludere brukernavn og passord, kodet i Base64.

Alternativer til grunnleggende autentisering inkluderer OAuth og JWT, som er mer komplekse, men gir større sikkerhet og fleksibilitet.

Når det gjelder implementeringsdetaljer i Ruby, konverterer `basic_auth`-metoden brukernavn og passord til en Base64-streng, og setter det i `Authorization`-feltet i HTTP-overskriften.

## Se også
- [Mer om net/http-biblioteket](https://ruby-doc.org/stdlib-3.0.1/libdoc/net/http/rdoc/Net/HTTP.html)
- [RFC 7617 - The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)

---