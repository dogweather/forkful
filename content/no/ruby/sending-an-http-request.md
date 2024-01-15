---
title:                "Å sende en http-forespørsel"
html_title:           "Ruby: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sende HTTP-forespørsler (HTTP requests) er en viktig del av å utvikle nettsider og applikasjoner. Det gjør det mulig for oss å kommunisere med ulike servere og få tilgang til ulike tjenester og data.

## Hvordan

For å sende en HTTP-forespørsel i Ruby, kan du bruke Net::HTTP biblioteket. Først må du importere biblioteket:

```Ruby
require 'net/http'
```

Deretter kan du bruke `Net::HTTP.start` metoden til å åpne en forbindelse til URL-en du ønsker å sende en forespørsel til:

```Ruby
url = URI.parse('https://example.com')
connection = Net::HTTP.start(url.host, url.port)
```

Nå kan du bruke `Net::HTTP::Get` klasse til å lage en GET forespørsel og sende den gjennom forbindelsen:

```Ruby
request = Net::HTTP::Get.new(url)
response = connection.request(request)
puts response.body # output: <h1>Hello world!</h1>
```

Du kan også sende andre typer HTTP-forespørsler som `Net::HTTP::Post`, `Net::HTTP::Put` og `Net::HTTP::Delete` ved å endre på metoden i `Net::HTTP::Get.new()`.

## Dypdykk

Man kan også spesifisere flere parametere når man sender en HTTP-forespørsel, som for eksempel headers og body. Her er et eksempel på hvordan man kan legge til headers i en GET forespørsel:

```Ruby
request = Net::HTTP::Get.new(url)
request['Content-Type'] = 'application/json'
response = connection.request(request)
```

Du kan også bruke `Net::HTTP::Post.new()` for å sende en POST forespørsel med en body som for eksempel et JSON-objekt. For å gjøre dette må du først konvertere objektet til en streng vha. `#to_s` metoden, og deretter sette det som `request.body`:

```Ruby
request = Net::HTTP::Post.new(url)
request['Content-Type'] = 'application/json'
payload = { name: 'John', age: 28 }.to_s
request.body = payload
response = connection.request(request)
puts response.body # output: { message: 'User created successfully' }
```

## Se også

- [Net::HTTP dokumentasjon](https://ruby-doc.org/stdlib-3.0.0/libdoc/net/http/rdoc/Net/HTTP.html)
- [Ruby for nybegynnere](https://ruby-for-beginners.rubymonstas.org/)
- [Ruby Gems](https://rubygems.org/)