---
title:                "Ruby: Send en http-forespørsel"
simple_title:         "Send en http-forespørsel"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Hvorfor bruke HTTP-forespørsler i Ruby-programmering

HTTP-forespørsler er en viktig del av Ruby-programmering, for å kunne kommunisere med andre nettsider og tjenester. Det lar deg hente og sende data fra og til eksterne kilder, og integrere din egen kode med andre API-er. Å kunne få tilgang til disse eksterne tjenestene åpner opp for et bredere og kraftigere utvalg av funksjonalitet i dine programmer.

## Hvordan gjøre det

For å sende en HTTP-forespørsel i Ruby, må du først installere og importere et bibliotek som heter "net/http". Dette er standardbiblioteket for å håndtere HTTP-forespørsler i Ruby.

Deretter kan du bruke metoden "get" for å sende en forespørsel til en bestemt URL-adresse, og lagre responsen i en variabel for senere bruk. Her er et eksempel på hvordan det kan se ut:

```ruby
require 'net/http'
response = Net::HTTP.get('www.example.com', '/api/customers')
puts response.body
```

Dette vil sende en GET-forespørsel til example.com sin /api/customers-sti og lagre svaret i variabelen "response". Deretter skriver vi ut innholdet i responsen ved hjelp av "puts" kommandoen.

Du kan også sende andre typer forespørsler, som for eksempel POST- eller PUT-forespørsler ved å bruke tilsvarende metoder. Her er et eksempel på hvordan du kan sende en POST-forespørsel med Net::HTTP:

```ruby
require 'net/http'
uri = URI('www.example.com')
request = Net::HTTP::Post.new(uri)
request.set_form_data({username: 'Testuser', password: '12345'})
response = Net::HTTP.start(uri.hostname, uri.port) do |http|
  http.request(request)
end
puts response.body
```

Dette eksempelet viser hvordan du kan sende en POST-forespørsel til example.com og legge til data i forespørselen ved hjelp av "set_form_data" metoden. Deretter lagrer vi responsen i variabelen "response" og skriver ut innholdet med "puts" kommandoen.

## Dypdykk

For å forstå mer om hvordan du kan bruke HTTP-forespørsler i Ruby, er det viktig å ha kunnskap om ulike typer forespørsler og responskoder. En forespørsel kan for eksempel ha en responskode som indikerer om forespørselen ble utført suksessfullt eller ikke. Det er også mulig å sende forespørsler med ulike typer autorisasjon, for eksempel basic authentication eller token authentication.

I tillegg er det viktig å forstå hvordan du kan håndtere eventuelle feil som kan oppstå under en HTTP-forespørsel. Du kan for eksempel bruke "begin" og "rescue" blokker for å fange opp og håndtere feil, slik at programmet ditt ikke stopper helt hvis noe går galt.

## Se også

- [Ruby net/http bibilotek](https://ruby-doc.org/stdlib-2.5.1/libdoc/net/http/rdoc/Net/HTTP.html)
- [HTTP-forespørsler i Ruby on Rails](https://guides.rubyonrails.org/v5.2.3/getting_started.html#using-form-helpers)
- [Intro til HTTP-forespørsler i Ruby](https://www.digitalocean.com/community/tutorials/how-to-use-net-http-with-ssl-to-connect-to-an-ssl-protected-web-page)