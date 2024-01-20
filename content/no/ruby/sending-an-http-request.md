---
title:                "Å sende en http-forespørsel"
html_title:           "C++: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

---

## Hva & Hvorfor?

Å sende en HTTP-forespørsel er handlingen med å spørre en server om en bestemt ressurs, som en nettside eller en fil. Programmerere gjør dette for å hente data, sende data, eller interagere med eksterne tjenester og APIer.

## Hvordan:

Her er et enkelt eksempel på hvordan sende en GET HTTP-forespørsel til en server ved hjelp av 'net/http' biblioteket i Ruby:

```Ruby
require 'net/http'

url = URI("http://example.com")
response = Net::HTTP.get(url)

puts response
```

Koden ovenfor vil skrive ut innholdet på siden 'http://example.com'. 

Hvis du vil sende data til serveren, bruker du en POST-forespørsel. Her er et eksempel:

```Ruby
require 'net/http'

url = URI("http://example.com")
http = Net::HTTP.new(url.host, url.port)

request = Net::HTTP::Post.new(url)
request["Content-Type"] = "application/json"
request.body = '{"data": "test"}'

response = http.request(request)
puts response.read_body
```

## Dypdykk

Det historiske konteksten for sending av HTTP-forespørsler går tilbake til opprettelsen av World Wide Web. HTTP, eller Hypertext Transfer Protocol, var det primære verktøyet for kommunikasjon mellom klienter og servere.

Det finnes mange alternative metoder for å sende HTTP-forespørsler i Ruby, inkludert andre biblioteker som 'httparty' og 'rest-client'. Valget av metode avhenger ofte av spesifikke behov i prosjektet.

Når det gjelder implementeringsdetaljer, er en viktig ting å huske at forskjellige typer HTTP-forespørsler (GET, POST, DELETE, etc.) brukes for forskjellige formål. GET brukes til å hente informasjon, mens POST sender info og DELETE ser vekk med data.

## Se Også

Her er noen flere ressurser du kan sjekke ut for å lære mer om dette emnet:

- [Ruby Dokumentasjon om Net::HTTP](https://ruby-doc.org/stdlib-2.5.1/libdoc/net/http/rdoc/Net/HTTP.html)

- [RESTful APIer med Ruby: Frameworks and Tools](https://medium.com/swlh/restful-apis-with-ruby-gems-and-tools-962d81c6591d)

- [How HTTP Works](https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview)