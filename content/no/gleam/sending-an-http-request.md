---
title:                "Sending en http-forespørsel"
html_title:           "Gleam: Sending en http-forespørsel"
simple_title:         "Sending en http-forespørsel"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

##Hvorfor

Hvorfor skulle noen ønske å sende en HTTP-forespørsel? En HTTP-forespørsel er en måte å kommunisere med et nettsted eller en webtjeneste på, og det kan være nyttig for å hente informasjon eller utføre handlinger på en begrenset måte.

##Slik gjør du det

Det første steget for å sende en HTTP-forespørsel med Gleam er å importere biblioteket `httpc` og `gleam/http`, som er ansvarlig for å sende forespørselen og behandle svaret. Deretter må du opprette en instans av `httpc.Client` og angi URL-en du ønsker å sende forespørselen til. Til slutt kan du bruke funksjonen `send` for å sende forespørselen og få tilbake et svar.

```Gleam
import httpc
import gleam/http

client = httpc.Client.new()
url = "https://example.com"

response = client.send(gleam/http.get(url))

// Output:
// Ok(
//  Response(
//    status = 200,
//    headers = Headers([Bytes("content-type"), Bytes("text/html; charset=utf-8")]),
//    body = Response.Body.Content(Bytes([116, 101, 115, 116]))
//  )
// )
```

##Dykke dypere

Det finnes flere metoder for å sende en HTTP-forespørsel med Gleam, inkludert `post`, `put`, `delete` og `head`. Disse har alle lignende funksjonalitet som `get`-metoden, men hver av dem er optimalisert for å utføre en spesifikk type handling. I tillegg kan du også sende tilpassede headers og sette en timeout-verdi for forespørselen din.

##Se også

- Offisiell Gleam-dokumentasjon for HTTP-klienten: https://gleam.run/modules/gleam/http/latest/
- En enkel veiledning til HTTP-forespørsler med Gleam: https://medium.com/@IonicAshley/building-restful-services-in-gleam-9edbd04497e0