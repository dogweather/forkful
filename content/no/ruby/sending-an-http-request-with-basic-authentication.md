---
title:                "Sending en http-forespørsel med grunnleggende autentisering"
html_title:           "Ruby: Sending en http-forespørsel med grunnleggende autentisering"
simple_title:         "Sending en http-forespørsel med grunnleggende autentisering"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Når du sender en HTTP-forespørsel med grunnleggende autentisering, legger du til en brukernavn og passord i forespørselen for å sikre at bare autoriserte brukere kan få tilgang til dataene som forespørres. Dette brukes ofte når du kommuniserer med et API eller et system som krever autentisering.

Programmere gjør dette for å sikre at bare de som har korrekt autorisasjon kan få tilgang til dataene. Dette er en viktig sikkerhetspraksis for å beskytte sensitiv informasjon og hindre uautorisert tilgang.

## Slik gjør du:

```Ruby
require 'net/http'
require 'uri'

# Lag en URI for å sende HTTP-forespørselen
uri = URI('http://example.com/api/data')

# Lag en Net::HTTP instans og legg til brukernavn og passord for basic authentication
req = Net::HTTP::Get.new(uri)
req.basic_auth 'brukernavn', 'passord'

# Send forespørselen og lagre svaret i en variabel
res = Net::HTTP.start(uri.hostname, uri.port) {|http| http.request(req)}

# Skriv ut svaret
puts res.body
```

Ved å legge til brukenavn og passord i forespørselen, kan du sikre at bare autoriserte brukere får tilgang til dataene fra denne API-en.

## Dykk ned:

### Historisk kontekst:
HTTP basic authentication ble introdusert i HTTP/1.0 i 1996 for å sikre enkel autentisering på nettet. Det har blitt erstattet av mer avanserte autentiseringsmetoder, men er fortsatt i bruk og er en effektiv måte å sikre API-forespørsler på.

### Alternativer:
Avhengig av sikkerhetsbehovene dine, kan du også implementere SSL-sertifikater og mer avanserte autentiseringsprotokoller som OAuth for å sikre API-forespørsler.

### Implementeringsdetaljer:
Det finnes flere Ruby-gemmer som kan hjelpe deg med å sende HTTP-forespørsler med basic authentication, som Net::HTTP, Faraday og HTTParty. Disse gemmene abstraherer bort mye av arbeidet med å opprette og sende HTTP-forespørsler, slik at du enkelt kan legge til basic authentication-funksjonalitet i koden din.

## Se også:

- [Net::HTTP dokumentasjon](https://ruby-doc.org/stdlib-2.7.2/libdoc/net/http/rdoc/Net/HTTP.html)
- [Faraday](https://github.com/lostisland/faraday)
- [HTTParty](https://github.com/jnunemaker/httparty)