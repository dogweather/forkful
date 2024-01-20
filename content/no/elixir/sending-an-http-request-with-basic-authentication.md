---
title:                "Sende en http-forespørsel med grunnleggende autentisering"
html_title:           "Kotlin: Sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Sende en http-forespørsel med grunnleggende autentisering"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Send en HTTP-forespørsel med grunnleggende autentisering med Elixir

## Hva & Hvorfor?
En HTTP-forespørsel med grunnleggende autentisering er når du sender data over HTTP-protokollen med innloggingdetaljer som brukernavn og passord. Programmører gjør dette for å tilby sikker kommunikasjon med webservere.

## Hvordan gjøre det:
Her er et eksempel på hvordan man sender en HTTP GET-forespørsel med grunnleggende autentisering i Elixir ved bruk av HTTPoisons `basic_auth`-metode:

```Elixir
{:ok, response} = HTTPoison.get("http://example.com", [], basic_auth: {"username", "password"})
IO.inspect(response.body)
```
Du bør se den forespurte ressursen til output. Pass bare på at du erstatter "http://example.com", "username" og "password" med dine egentlige verdier.

## Dyp dykk
Grunnleggende autentisering er et gammelt konsept i webverdenen, som er definert i RFC 7617. Det tilbyr en enkel måte å beskytte ressurser på en webserver på. Alternativt kunne du også brukt Digest-autentisering, OAuth, eller Bearer Tokens avhengig av hva API-en støtter. Elixir bruker Erlang's :httpc modul under panseret for å håndtere HTTP-forespørsler, så egenskapene og begrensningene til :httpc gjelder også her.

## Se også
For flere detaljer om å jobbe med HTTP i Elixir, sjekk ut følgende ressurser:

- [HTTPoison GitHub repo](https://github.com/edgurgel/httpoison)
- [Elixir's :httpc Doc](https://erlang.org/doc/man/httpc.html)
- [RFC 7617 - The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)

Merk: Sørg for å ha ferskste versjon av Elixir samt HTTPoison biblioteket for å unngå eventuelle kompatibilitetsproblemer.