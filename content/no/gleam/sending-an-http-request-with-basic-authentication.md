---
title:                "Å sende en HTTP-forespørsel med grunnleggende autentisering"
date:                  2024-01-20T18:01:41.754462-07:00
model:                 gpt-4-1106-preview
simple_title:         "Å sende en HTTP-forespørsel med grunnleggende autentisering"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sende en HTTP-forespørsel med grunnleggende godkjenning betyr at vi gir brukernavn og passord slik at serveren kan sjekke om vi har tilgang. Vi gjør dette for å sikre trygg tilgang til ressurser på nettet.

## Hvordan gjøre det:
```gleam
import gleam/http
import gleam/httpc
import gleam/base64

pub fn send_request() {
  let url = "https://example.com/api"
  let username = "brukernavn"
  let password = "passord"
  let credentials = base64.encode(username ++ ":" ++ password)
  let headers = [http.Header("Authorization", "Basic " ++ credentials)]

  try response = httpc.send(http.Request(url, headers))
  io.debug(response) // For å se svaret fra serveren
}
```
Eksempel på output:
```gleam
Ok(#{
  body: "{\"status\":\"suksess\"}",
  status: 200,
  headers: [...],
  ...
})
```

## Dypdykk
I de tidlige dagene av internett var grunnleggende godkjenning standarden for å beskytte ressurser. Nå finnes det sikrere metoder som OAuth, men den grunnleggende godkjenningen er fortsatt i bruk for enkelhetens skyld. Med Gleam, som kompileres til Erlang VM, får vi en robust og sikker måte å håndtere HTTP-forespørsler på. Dette inkluderer godkjenning gjennom en header som koder brukerens legitimasjon med Base64.

## Se Også
- [Gleam HTTP documentation](https://hexdocs.pm/gleam_http/)
- [HTTP authentication: Basic and Digest access authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [Basics of Base64 encoding and decoding](https://base64.guru/learn/what-is-base64)
