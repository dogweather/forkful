---
title:                "Å sende en http-forespørsel"
html_title:           "C++: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sende en HTTP-forespørsel er en måte for programmer å kommunisere med nettressurser som nettsteder eller APIer. Programmerere gjør dette for å hente eller sende data fra/til disse ressursene.

## Hvordan gjøre det:
Gleam lar oss enkelt utføre HTTP-anrop via `gleam/httpc` pakken. Her er en enkel GET forespørsel:

```Gleam
import gleam/httpc
import gleam/uri.{Uri}

fn simple_get_request() {
  let Ok(response) = httpc.get(Uri.parse("https://gleam.run").unwrap())
  case response {
    Ok(response) -> httpc.Response.status(response)
    Error(_error) -> "Noe gikk galt"
  }
}
```
Denne koden vil hente webinnholdet fra "https://gleam.run" og returnere statuskoden. 

## Deep Dive
Historisk har HTTP-forespørsler blitt brukt som en standard metode for å kommunisere over nettet siden introduksjonen av World Wide Web. Sammen med utviklingen av programmeringsspråk og teknologier, har også metoder for å sende HTTP-forespørsler blitt mer strømlinjeformet og effektiv.

I Gleam, kan vi bruke Pakken `gleam/httpc` for å sende HTTP-forespørsler. Pakken gir et oppsett for å utføre de forskjellige typene av HTTP-anrop, GET, POST, DELETE, osv. Pakken styrer også alle detaljer rundt opprettelsen og håndteringen av forespørslene.

Alternativt kan programmerere også bruke bibliotekene `lhttpc` eller `hackney` for å sende HTTP-forespørsler i Gleam. Valget mellom disse bibliotekene beror ofte på personlig preferanse eller prosjektbehov.

## Se Også
- Lær mer om HTTP-forespørsler: [Link](https://www.w3schools.com/tags/ref_httpmethods.asp)