---
title:                "Å sende en http-forespørsel med grunnleggende autentisering"
html_title:           "Elixir: Å sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Å sende en http-forespørsel med grunnleggende autentisering"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor ville noen sende en HTTP-forespørsel med grunnleggende autentisering? Det kan være nyttig for å legge til et ekstra lag av sikkerhet til en nettapplikasjon. Basic authentication krever at brukeren gir et brukernavn og et passord for å få tilgang til en ressurs, og er derfor en enkel måte å beskytte sensitive data på.

## Hvordan

Å sende en HTTP-forespørsel med grunnleggende autentisering i Elixir er enkelt. Først må vi importere modulen for å sende HTTP-forespørsler, og deretter legge til autentiseringsinformasjon i headeren til forespørselen.

```elixir
# Importer HTTP-modulet
import HTTPoison

# Definer autentiseringsinformasjon
auth = {:basic, "brukernavn", "passord"}

# Send en GET-forespørsel med basic authentication
{:ok, response} = HTTPoison.get("https://eksmpel.com/api/ressurs", [auth: auth])

# Skriv ut statuskoden for responsen
IO.puts(response.status_code)
```

Dette vil sende en HTTP-forespørsel til "https://eksmpel.com/api/ressurs" og autentisere med brukernavn og passord som er gitt i "auth" variabelen. Hvis autentiseringen er vellykket, vil statuskoden til responsen være 200, ellers vil det være en annen feilkode.

## Dypdykk

HTTP-forespørsler med grunnleggende autentisering bruker en enkel autentiseringsprotokoll som krever brukernavn og passord som sendes i klartekst. Dette gjør det sårbart for avlytting, så det anbefales å bruke HTTPS i stedet for HTTP når du bruker basic authentication. En annen begrensning er at basic authentication ikke støtter utlogging, så brukeren må sende utloggingsforespørsel for å bli logget ut.

## Se også

* [HTTPoison dokumentasjon](https://github.com/edgurgel/httpoison)
* [Elixir HTTP-håndbok](https://hexdocs.pm/elixir/http.html)
* [Grunnleggende autentiseringsprotokoll](https://www.w3.org/Protocols/rfc2617/rfc2617.html)