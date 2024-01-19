---
title:                "Sende en http-forespørsel med grunnleggende autentisering"
html_title:           "Kotlin: Sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Sende en http-forespørsel med grunnleggende autentisering"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sende en HTTP-forespørsel med grunnleggende godkjenning er prosessen hvor en programvare sender data til en server og autentiserer ved hjelp av brukernavn og passord. Programvareutviklere gjør dette for å sikre at bare autoriserte parter får tilgang til spesifikke ressurser.

## Slik gjør du:
For å sende en HTTP-forespørsel med grunnleggende autentisering i Gleam, kan vi bruke `httpc:request` funksjonen som vist nedenfor:

```Gleam
let response =
  httpc
  |> httpc.basic_auth("user", "password")
  |> httpc.post(
    "http://www.example.com",
    body: "Some body",
    headers: [tuple("Content-Type", "application/x-www-form-urlencoded")],
  )
```
Når du kjører detta kode, sender du en POST-request med basic autentisering (brukernavn og passord) til `http://www.example.com`.

## Dypdykk
Å sende HTTP-forespørsler med grunnleggende autentisering har blitt brukt siden opprettelsen av weben, da det første gang ble introdusert i HTTP/1.0-spesifikasjonen.

En alternativ måte å gjøre dette på kan være bruk av OAuth, som gir en mer sikker metode for autentisering. Men, grunnleggende autentisering fortsetter å bli brukt for sin enkelhet og fordi det er supportert av alle HTTP-klienter.

Implementeringsdetaljer er relativt enkle med Gleam. Når httpc-funksjonen brukes med `basic_auth`-metoden, blir brukernavnet og passordet satt i `Authorization`-headeren i HTTP-requesten. Dataene må være base64-kodet, noe som Gleam håndterer i bakgrunnen.

## Se også
Her er noen nyttige lenker for ytterligere informasjon:

- Gleam HTTP client modul: https://hexdocs.pm/gleam_stdlib/gleam/httpc.html
- HTTP/1.0-spesifikasjon: https://www.w3.org/Protocols/HTTP/1.0/spec.html
- OAuth-spesifikasjon: https://oauth.net/