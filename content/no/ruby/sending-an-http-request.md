---
title:                "Sende en http-forespørsel"
html_title:           "Ruby: Sende en http-forespørsel"
simple_title:         "Sende en http-forespørsel"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

#Hva & Hvorfor?

Sending av HTTP forespørsler er en essensiell del av nettutvikling. Det er en måte å kommunisere med andre servere og få tilgang til og manipulere informasjon. Programmere gjør dette for å hente data fra API-er, lage dynamiske nettsteder og bygge nettapplikasjoner.

#Hvordan?

For å sende en HTTP forespørsel i Ruby, kan du bruke innebygde metodene i Net::HTTP biblioteket. Her er et eksempel på hvordan du kan hente data fra en nettside og skrive ut svaret:

```
require 'net/http'

url = URI('https://www.example.com')
response = Net::HTTP.get_response(url)
puts response.body
```

I dette eksempelet bruker vi 'net/http' modulen for å opprette en URL-objekt og utføre en GET-forespørsel. Svaret blir lagret i en variabel og deretter skrevet ut ved hjelp av puts-metoden.

#Dypdykk

HTTP (Hypertext Transfer Protocol) er et klient-server protokoll som brukes på verdensveb for å sende og motta data. Det ble utviklet på 1990-tallet som et enkelt og universelt protokoll for å kommunisere mellom forskjellige systemer.

I tillegg til Net::HTTP, finnes det også alternative måter å sende HTTP forespørsler i Ruby, som HTTParty og RestClient biblioteker. Disse bibliotekene gir enklere syntaks og flere funksjoner for å håndtere HTTP forespørsler.

For å sende en mer kompleks forespørsel, kan du også bruke Net::HTTP::Request-objekter som lar deg angi HTTP-metode, headers og kropp av forespørselen.

#Se også

- Dokumentasjon for Net::HTTP: https://ruby-doc.org/stdlib-2.6.5/libdoc/net/http/rdoc/Net/HTTP.html
- HTTParty bibliotek: https://github.com/jnunemaker/httparty
- RestClient bibliotek: https://github.com/rest-client/rest-client