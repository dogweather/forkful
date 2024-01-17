---
title:                "Å sende en http-forespørsel"
html_title:           "Elixir: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Sending av HTTP-forespørsler er en essensiell del av moderne webutvikling. Det betyr at programmene våre kan kommunisere med andre datamaskiner og hente informasjon fra internett. Det gjør programmererens liv mye enklere og gjør at vi kan bygge kraftige applikasjoner som kan tilfredsstille våre behov.

## Slik gjør du:

```Elixir
# Bruk HTTPoison biblioteket
# For å gjøre en GET-forespørsel:
response = HTTPoison.get("https://www.example.com")
# For å gjøre en POST-forespørsel:
response = HTTPoison.post("https://www.example.com/users", body: %{name: "John", age: 25})
# Hent ut statuskoden
response.status_code
# Hent ut responsens kropp
response.body
```

## Dykk dypere:

HTTP-protokollen ble utviklet for å tillate kommunikasjon mellom klienter og servere på internett. Første versjon ble introdusert i 1991 og det har vært flere versjoner siden. Det finnes flere alternativer for å sende HTTP-forespørsler i Elixir, men HTTPoison er et populært valg på grunn av sin enkle og intuitive API. Det er også verdt å nevne at Elixirs innebygde "httpc" bibliotek gir mulighet for å sende HTTP-forespørsler uten å installere tredjeparts biblioteker.

## Se også:

- [HTTPoison dokumentasjon](https://hexdocs.pm/httpoison/HTTPoison.html)
- [Elixir HTTP klienter sammenlignet](https://dev.to/castore/elixir-http-clients-compared-1i3a)