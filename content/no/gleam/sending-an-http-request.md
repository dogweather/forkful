---
title:                "Å sende en HTTP-forespørsel"
html_title:           "Gleam: Å sende en HTTP-forespørsel"
simple_title:         "Å sende en HTTP-forespørsel"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å sende en HTTP forespørsel betyr å be om data fra en nettside eller server. Dette kan være nyttig for å få informasjon, for eksempel værvarsler eller nyheter, eller for å sende data til en tjeneste, for eksempel å legge til et bilde på en nettside. Programører bruker HTTP-forespørsler for å lage programmer som kan kommunisere med andre nettsteder og tjenester.

## Hvordan:

``Gleam`` har innebygde funksjoner som gjør det enkelt å sende HTTP-forespørsler. Vi kan bruke ``httpc`` modulen til å opprette en forespørsel og få svar fra en server.

Først importerer vi ``httpc`` modulen:

```Gleam
import httpc
```

Vi kan nå bruke funksjonen ``send`` til å opprette forespørselen. Vi må gi den en URL og en liste med parametere, som kan være nyttige for å spesifisere hvilken type forespørsel vi ønsker å sende. Her er et eksempel på en GET-forespørsel:

```Gleam 
let req = httpc.send("https://example.com", [ httpc.method.get ])
```

Vi kan også legge til en ``headers`` parameter for å sende spesifikke headere med forespørselen. For å få responsen fra serveren, bruker vi funksjonen ``response``:

```Gleam
let resp = req |> httpc.response
```

Vi kan nå bruke forskjellige funksjoner fra ``httpc`` modulen til å jobbe med responsen, for eksempel ``status_code``, ``headers`` og ``body``.

## Dykk dypere:

HTTP-forespørsler har vært en integrert del av internett siden 1991. I dag er det flere alternativer til HTTP, som for eksempel HTTPS og SPDY. Disse protokollene gir bedre sikkerhet og effektivitet.

Gleam bruker en asynkron tilnærming til å håndtere HTTP-forespørsler, noe som betyr at det ikke blokkerer programmet mens det venter på et svar fra serveren. Dette gjør det mulig for programmet å håndtere flere forespørsler samtidig.

## Se også:

- [Offisiell Dokumentasjon](https://gleam.run/documentation/http/): Lær mer om hvordan du bruker Gleam sin HTTP-modul.
- [HTTP Clients Explained](https://www.cloudflare.com/learning/ddos/glossary/http-client/): En dyptgående forklaring på hva en HTTP-klient er og hvordan den fungerer.
- [The History of HTTP](https://www.w3.org/Protocols/rfc2616/rfc2616.html): Lær om utviklingen av HTTP gjennom årene.