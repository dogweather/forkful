---
title:                "Å sende en HTTP-forespørsel"
date:                  2024-01-20T17:59:47.449891-07:00
model:                 gpt-4-1106-preview
simple_title:         "Å sende en HTTP-forespørsel"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sende en HTTP-forespørsel innebærer å be om data fra en server. Programmerere gjør dette for å hente nettressurser, kommunisere med API-er og drive nettsider.

## Hvordan:
```gleam
import gleam/httpc
import gleam/http.{Request, Method}
import gleam/result.{Ok, Error}

pub fn main() {
  let request = Request(
    method: Method::Get,
    url: "http://example.com",
    headers: [],
    body: "",
  )

  match httpc.send(request) {
    Ok(response) -> 
      io.println("Hentet data: \n{response.body}")
    Error(_error) ->
      io.println("Kunne ikke hente data.")
  }
}
```
Resultat:
```
Hentet data: 
<html>...</html>
```
Eller en feilmelding, avhengig av situasjonen.

## Dypdykk
Tidlig i webutviklingens historie sendte vi HTTP-forespørsler via formularer og lenker. Med nye teknologier som Ajax forbedret vi hastighet og brukeropplevelsen. 

I Gleam kan vi bruke standardbiblioteket, som `gleam/httpc`, for å utføre disse forespørslene. Det er også andre pakker som `gleam/http`, som gir ulike nivåer av abstraksjon og brukervennlighet.

Senking av HTTP-forespørsler gjort i Gleam handler om å lage et `Request` objekt og bruke `httpc.send` funksjonen til å motta et `Response` objekt eller en feil. Gleam behandler type sikkerhet seriøst, så du er aldri i tvil om hva din funksjon gir eller krever.

## Se Også
- Gleam HTTP klientdokumentasjon: https://hexdocs.pm/gleam_http_client
- HTTP spesifikasjon: https://www.ietf.org/rfc/rfc2616.txt
- "Let's Talk About HTTP" Gleam forum tråd: https://github.com/gleam-lang/gleam/discussions